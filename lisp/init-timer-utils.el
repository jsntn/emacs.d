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


(defun my-calculate-tomorrow-date ()
  "Calculate tomorrow's date and return it as a list of (day month year).

Version: 2023-09-05"
  (interactive)
  (let* ((current-time (decode-time (current-time)))
	 (current-day (nth 3 current-time))
	 (current-month (nth 4 current-time))
	 (current-year (nth 5 current-time))
	 (tomorrow-day (1+ current-day))
	 (tomorrow-month (if (> tomorrow-day
		 (calendar-last-day-of-month current-month current-year))
			     (1+ current-month)
			   current-month))
	 (tomorrow-year (if (> tomorrow-month 12)
			    (1+ current-year)
			  current-year)))
    (setq tomorrow-day (if (> tomorrow-day
			      (calendar-last-day-of-month current-month current-year))
			   1
			 tomorrow-day))
    ;; (message "Tomorrow's date is: %d-%02d-%02d" tomorrow-year tomorrow-month tomorrow-day)
    (list tomorrow-day tomorrow-month tomorrow-year)))

(defun my-current-time-in-minutes ()
  "Return the current time in minutes since midnight.

Version: 2023-09-05"
  (let* ((current-time (decode-time (current-time)))
	 (current-hour (nth 2 current-time))
	 (current-minute (nth 1 current-time))
	 (current-time-in-minutes (+ (* current-hour 60) current-minute)))
    current-time-in-minutes))



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

  (let* ((current-time-in-minutes (my-current-time-in-minutes))
	 (scheduled-time-in-minutes (+ (* hour 60) minute)))
    (if (<= scheduled-time-in-minutes current-time-in-minutes)
	;; If the scheduled time is before or equal to the current time, set schedule for tomorrow
	(let* ((tomorrow-date (my-calculate-tomorrow-date))
	       (tomorrow-time
		(encode-time 0 minute hour
			     (car tomorrow-date)
			     (cadr tomorrow-date)
			     (caddr tomorrow-date))))
	  (run-at-time tomorrow-time
		       (* 60 60 24)
		       (intern task-name)))
      (setq task-function-timer
	    (run-at-time (format "%02d:%02d" hour minute)
			 (* 60 60 24)
			 task-function)))))




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






(defun my-schedule-task-at-specific-min-between-hour
    (start-hour end-hour specific-minutes base-task-function)
  "Schedule tasks to run at specific minutes between two hours.

  This function schedules tasks with different specific minutes between the given
  start and end hours. The task names will have the format 'base-task-function_hour-min'.

  Args:
      start-hour (integer): The starting hour (0 to 23) of the time range.
      end-hour (integer): The ending hour (0 to 23) of the time range.
      specific-minutes (list): A list of specific minutes (0 to 59) at which the tasks should run.
      base-task-function (function): The base function to be executed when the tasks are scheduled.

  Returns:
      None: The tasks are scheduled to run using the 'run-at-time' function.

  Note:
      The 'run-at-time' function is used to schedule the tasks, and it may not guarantee
      exact timing due to various factors such as system load and other scheduled tasks.

      Example:
      (my-schedule-task-at-specific-min-between-hour 10 17 '(0 15 30 45) 'my-task-function)
      This will schedule tasks to execute 'my-task-function' at minutes 0, 15, 30, and 45
      between 10:00 AM and 5:00 PM.

Version: 2023-09-04
Updated: 2023-09-05"
  ;; Define the task name format
  (let ((task-name-format
	 (format "%s_%%02d-%%02d" (symbol-name base-task-function))))
    ;; Get the current time in minutes since midnight
    (let* ((current-time (decode-time (current-time)))
	   (current-hour (nth 2 current-time))
	   (current-minute (nth 1 current-time))
	   (current-time-in-minutes (+ (* current-hour 60) current-minute)))
      ;; Loop through hours and minutes to schedule tasks
      (dotimes (hour-counter (- end-hour start-hour))
	(let ((current-hour (+ start-hour hour-counter)))
	  (dolist (minute specific-minutes)
	    ;; Calculate the scheduled task time in minutes since midnight
	    (let ((scheduled-time-in-minutes (+ (* current-hour 60) minute)))
	      ;; Construct the full task name with hour and minute
	      (let ((task-name (format task-name-format current-hour minute)))
		;; Cancel existing timer with the same task name
		(my-cancel-existing-timer (intern task-name))
		;; Check if the scheduled time is ahead of the current time
		;; Define the new task function using defalias
		(defalias (intern task-name)
		  `(lambda ()
		     ,(format "Scheduled task: %s" (symbol-name base-task-function))
		     (funcall ',base-task-function)))
		(if (<= scheduled-time-in-minutes current-time-in-minutes)
		    ;; If the scheduled time is before or equal to the current time, set schedule for tomorrow
		    (let* ((tomorrow-date (my-calculate-tomorrow-date))
			   (tomorrow-time
			    (encode-time 0 minute current-hour
					 (car tomorrow-date)
					 (cadr tomorrow-date)
					 (caddr tomorrow-date))))
		      (run-at-time tomorrow-time
				   (* 60 60 24)
				   (intern task-name)))
		  ;; Schedule the new task
		  (run-at-time (format "%02d:%02d" current-hour minute)
			       (* 60 60 24)
			       (intern task-name)))))))))))










(provide 'init-timer-utils)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-timer-utils.el ends here
