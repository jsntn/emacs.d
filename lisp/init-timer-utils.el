;;; init-timer-utils.el --- timer utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(defun my-cancel-existing-timer (timer-function)
  "Cancel an existing active timer with the given TIMER-FUNCTION.

Version: 2023-08-15"
  (dolist (timer timer-list)
    (when (equal (timer--function timer) timer-function)
      (cancel-timer timer)
      (setq timer-list (delq timer timer-list)))))

(defun my-schedule-task-every-day (hour minute task-function)
  "Schedule a task to run every day at a specific time.

  This function schedules the given task function to run at the specified hour
  and minute every day.

  Args:
      hour (integer): The hour of the day (0 to 23) at which the task should run.
      minute (integer): The minute of the hour (0 to 59) at which the task should run.
      task-function (function): The function to be executed when the scheduled time is reached.

  Returns:
      None: The task is scheduled to run using the 'run-at-time' function.

  Note:
      The 'run-at-time' function is used to schedule the task, and it may not guarantee
      exact timing due to various factors such as system load and other scheduled tasks.

      Example:
      (my-schedule-task-every-day 15 30 'my-task-function)
      This will schedule 'my-task-function' to run every day at 3:30 PM.

Version: 2023-08-15"
  (my-cancel-existing-timer task-function)

  (setq task-function-timer
        (run-at-time (format "%02d:%02d" hour minute)
                     (* 60 60 24)
                     task-function)))




(defun my-schedule-task-every-x-secs (seconds task-function)
  "Schedule a task to run every x seconds.
  (my-cancel-existing-timer task-function)

Version: 2023-08-28"
  (my-cancel-existing-timer task-function)

  (setq task-function-timer
	(run-at-time 0 ; the task should start immediately
		     seconds
		     task-function)))


(defun my-schedule-task-every-x-mins (minutes task-function)
  "Schedule a task to run every x mins.

Version: 2023-08-19"
  (my-cancel-existing-timer task-function)

  (setq task-function-timer
	(run-at-time 0 ; the task should start immediately
		     (* minutes 60)
		     task-function)))




(defun my-schedule-task-on-day-of-week (day-of-week hour minute task-function)
  "Schedule a task to run at a specific time on a particular day of the week.

  This function calculates the next occurrence of the specified day of the week
  and schedules the given task function to run at the specified hour and minute
  of that day.

  Args:
      day-of-week (integer): The desired day of the week (0 = Sunday, 1 = Monday, ..., 6 = Saturday).
      hour (integer): The hour of the day (0 to 23) at which the task should run.
      minute (integer): The minute of the hour (0 to 59) at which the task should run.
      task-function (function): The function to be executed when the scheduled time is reached.

  Returns:
      None: The task is scheduled to run using the 'run-at-time' function.

  Note:
      This function calculates the next occurrence of the specified day of the week
      based on the current date and time and then schedules the task to run on that
      calculated date and time.

      It is important to note that the 'run-at-time' function is used to schedule the task,
      and it may not guarantee exact timing due to various factors such as system load
      and other scheduled tasks.

      Example:
      (my-schedule-task-on-day-of-week 3 15 30 'my-task-function)
      This will schedule 'my-task-function' to run every Wednesday at 3:30 PM.

Version 2023-08-15"
  (let* ((current-time (current-time))
	 (decoded-time (decode-time current-time))
	 (current-day-of-week (nth 6 decoded-time))
	 (days-until-desired-day (mod (+ day-of-week (- current-day-of-week)) 7))
	 (desired-date (decode-time
			(time-add current-time (seconds-to-time
						(* days-until-desired-day 24 60 60)))))
	 (desired-day (nth 3 desired-date))
	 (desired-month (nth 4 desired-date))
	 (desired-year (nth 5 desired-date)))

    (my-cancel-existing-timer task-function)

    (run-at-time
     (encode-time 0 minute hour desired-day desired-month desired-year)
     (* 7 24 60 60)
     task-function)))




(defun my-schedule-task-in-x-mins (minutes task-function)
  "Schedule a task to run in x mins.

Version: 2023-08-19"
  (my-cancel-existing-timer task-function)

  (setq task-function-timer
	(run-at-time (format "%s min" minutes)
		     nil
		     task-function)))





(provide 'init-timer-utils)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-timer-utils.el ends here