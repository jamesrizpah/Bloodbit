;; ReputationRecovery.clar - Structured rehabilitation program for users with low reputation
;; Provides mentorship, tasks, and community oversight for reputation recovery

(define-constant contract-owner tx-sender)
(define-constant err-insufficient-reputation (err u300))
(define-constant err-already-enrolled (err u301))
(define-constant err-not-enrolled (err u302))
(define-constant err-invalid-mentor (err u303))
(define-constant err-task-not-found (err u304))
(define-constant err-task-already-completed (err u305))
(define-constant err-program-not-found (err u306))
(define-constant err-unauthorized (err u307))
(define-constant err-program-completed (err u308))

;; Data variables
(define-data-var next-program-id uint u0)
(define-data-var next-task-id uint u0)

;; Recovery program enrollment
(define-map recovery-programs uint
  {
    participant: principal,
    mentor: (optional principal),
    start-reputation: int,
    current-reputation: int,
    tasks-completed: uint,
    tasks-required: uint,
    program-start: uint,
    program-duration: uint,
    status: (string-ascii 20),
    improvement-target: int
  }
)

;; Recovery tasks for rehabilitation
(define-map recovery-tasks uint
  {
    program-id: uint,
    task-type: (string-ascii 30),
    description: (string-ascii 100),
    reputation-reward: int,
    completed: bool,
    completed-at: (optional uint),
    verified-by: (optional principal)
  }
)

;; Mentor assignments and qualifications
(define-map mentor-registry principal
  {
    min-reputation: int,
    active: bool,
    mentees-helped: uint,
    success-rate: uint,
    registered-at: uint
  }
)

;; Progress tracking for programs
(define-map program-progress uint
  {
    weekly-goals: uint,
    weekly-completed: uint,
    community-votes: uint,
    mentor-approval: bool,
    progress-notes: (string-ascii 200)
  }
)

;; User's recovery program lookup
(define-map user-recovery-lookup principal uint)

;; Read-only functions
(define-read-only (get-recovery-program (program-id uint))
  (map-get? recovery-programs program-id)
)

(define-read-only (get-user-program (user principal))
  (match (map-get? user-recovery-lookup user)
    program-id (map-get? recovery-programs program-id)
    none
  )
)

(define-read-only (get-mentor-info (mentor principal))
  (map-get? mentor-registry mentor)
)

(define-read-only (get-recovery-task (task-id uint))
  (map-get? recovery-tasks task-id)
)

(define-read-only (is-eligible-for-recovery (user principal))
  (let ((user-reputation (unwrap! (contract-call? .user-reputation-system get-reputation user) false)))
    (< (get score user-reputation) 0)
  )
)

;; Register as a mentor (requires good reputation)
(define-public (register-as-mentor)
  (let ((mentor-reputation (unwrap! (contract-call? .user-reputation-system get-reputation tx-sender) err-insufficient-reputation)))
    (if (>= (get score mentor-reputation) 50)
      (begin
        (map-set mentor-registry tx-sender
          {
            min-reputation: (get score mentor-reputation),
            active: true,
            mentees-helped: u0,
            success-rate: u0,
            registered-at: stacks-block-height
          }
        )
        (ok true)
      )
      err-insufficient-reputation
    )
  )
)

;; Enroll in recovery program
(define-public (enroll-in-recovery)
  (let ((user-reputation (unwrap! (contract-call? .user-reputation-system get-reputation tx-sender) err-insufficient-reputation))
        (existing-program (map-get? user-recovery-lookup tx-sender))
        (program-id (var-get next-program-id)))
    (if (and 
        (< (get score user-reputation) 0)
        (is-none existing-program))
      (let ((improvement-target (+ (get score user-reputation) 20)))  ;; Target +20 reputation
        (begin
          (map-set recovery-programs program-id
            {
              participant: tx-sender,
              mentor: none,
              start-reputation: (get score user-reputation),
              current-reputation: (get score user-reputation),
              tasks-completed: u0,
              tasks-required: u5,
              program-start: stacks-block-height,
              program-duration: u1440,  ;; 10 days
              status: "active",
              improvement-target: improvement-target
            }
          )
          (map-set user-recovery-lookup tx-sender program-id)
          (create-initial-tasks program-id)
          (var-set next-program-id (+ program-id u1))
          (ok program-id)
        )
      )
      (if (is-some existing-program) err-already-enrolled err-insufficient-reputation)
    )
  )
)

;; Assign mentor to recovery program
(define-public (assign-mentor (program-id uint) (mentor principal))
  (let ((program (unwrap! (map-get? recovery-programs program-id) err-program-not-found))
        (mentor-info (unwrap! (map-get? mentor-registry mentor) err-invalid-mentor)))
    (if (and 
        (is-eq tx-sender (get participant program))
        (get active mentor-info)
        (is-none (get mentor program)))
      (begin
        (map-set recovery-programs program-id
          (merge program { mentor: (some mentor) })
        )
        (ok true)
      )
      err-unauthorized
    )
  )
)

;; Complete a recovery task
(define-public (complete-task (task-id uint))
  (let ((task (unwrap! (map-get? recovery-tasks task-id) err-task-not-found))
        (program (unwrap! (map-get? recovery-programs (get program-id task)) err-program-not-found)))
    (if (and 
        (is-eq tx-sender (get participant program))
        (not (get completed task)))
      (begin
        (map-set recovery-tasks task-id
          (merge task { 
            completed: true,
            completed-at: (some stacks-block-height)
          })
        )
        (try! (contract-call? .user-reputation-system upvote tx-sender))
        (update-program-progress (get program-id task))
        (ok true)
      )
      err-task-already-completed
    )
  )
)

;; Mentor verifies task completion
(define-public (verify-task (task-id uint))
  (let ((task (unwrap! (map-get? recovery-tasks task-id) err-task-not-found))
        (program (unwrap! (map-get? recovery-programs (get program-id task)) err-program-not-found)))
    (match (get mentor program)
      mentor-principal
        (if (and 
            (is-eq tx-sender mentor-principal)
            (get completed task)
            (is-none (get verified-by task)))
          (begin
            (map-set recovery-tasks task-id
              (merge task { verified-by: (some tx-sender) })
            )
            (reward-completion (get program-id task) (get reputation-reward task))
            (ok true)
          )
          err-unauthorized
        )
      err-invalid-mentor
    )
  )
)

;; Community member can support recovery (limited voting)
(define-public (support-recovery (program-id uint))
  (let ((program (unwrap! (map-get? recovery-programs program-id) err-program-not-found))
        (supporter-reputation (unwrap! (contract-call? .user-reputation-system get-reputation tx-sender) err-insufficient-reputation))
        (progress (default-to 
          { weekly-goals: u0, weekly-completed: u0, community-votes: u0, mentor-approval: false, progress-notes: "" }
          (map-get? program-progress program-id)
        )))
    (if (>= (get score supporter-reputation) 10)
      (begin
        (map-set program-progress program-id
          (merge progress { community-votes: (+ (get community-votes progress) u1) })
        )
        (if (>= (get community-votes progress) u3)
          (try! (contract-call? .user-reputation-system upvote (get participant program)))
          true
        )
        (ok true)
      )
      err-insufficient-reputation
    )
  )
)

;; Graduate from recovery program
(define-public (graduate-program)
  (let ((program-id (unwrap! (map-get? user-recovery-lookup tx-sender) err-not-enrolled))
        (program (unwrap! (map-get? recovery-programs program-id) err-program-not-found))
        (current-reputation (get score (unwrap! (contract-call? .user-reputation-system get-reputation tx-sender) err-insufficient-reputation))))
    (if (and 
        (>= current-reputation (get improvement-target program))
        (>= (get tasks-completed program) (get tasks-required program)))
      (begin
        (map-set recovery-programs program-id
          (merge program { status: "completed" })
        )
        (map-delete user-recovery-lookup tx-sender)
        (reward-mentor program-id)
        (ok true)
      )
      err-program-not-found
    )
  )
)

;; Private helper functions
(define-private (create-initial-tasks (program-id uint))
  (let ((task-id-1 (var-get next-task-id))
        (task-id-2 (+ task-id-1 u1))
        (task-id-3 (+ task-id-1 u2)))
    (begin
      (map-set recovery-tasks task-id-1
        { program-id: program-id, task-type: "community-service", description: "Help other users in forums", 
          reputation-reward: 2, completed: false, completed-at: none, verified-by: none })
      (map-set recovery-tasks task-id-2
        { program-id: program-id, task-type: "skill-sharing", description: "Share knowledge or tutorial", 
          reputation-reward: 3, completed: false, completed-at: none, verified-by: none })
      (map-set recovery-tasks task-id-3
        { program-id: program-id, task-type: "peer-review", description: "Complete 3 peer reviews", 
          reputation-reward: 2, completed: false, completed-at: none, verified-by: none })
      (var-set next-task-id (+ task-id-1 u3))
    )
  )
)

(define-private (update-program-progress (program-id uint))
  (let ((program (unwrap! (map-get? recovery-programs program-id) err-program-not-found)))
    (map-set recovery-programs program-id
      (merge program { tasks-completed: (+ (get tasks-completed program) u1) })
    )
  )
)

(define-private (reward-completion (program-id uint) (reward int))
  (let ((program (unwrap! (map-get? recovery-programs program-id) err-program-not-found)))
    (begin
      (try! (contract-call? .user-reputation-system upvote (get participant program)))
      (ok true)
    )
  )
)

(define-private (reward-mentor (program-id uint))
  (let ((program (unwrap! (map-get? recovery-programs program-id) err-program-not-found)))
    (match (get mentor program)
      mentor-principal
        (begin
          (try! (contract-call? .user-reputation-system upvote mentor-principal))
          (let ((mentor-info (unwrap! (map-get? mentor-registry mentor-principal) err-invalid-mentor)))
            (map-set mentor-registry mentor-principal
              (merge mentor-info { mentees-helped: (+ (get mentees-helped mentor-info) u1) })
            )
          )
        )
      false
    )
  )
)