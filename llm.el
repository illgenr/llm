;;; llm.el --- EMACS porcelain for the program LLM  -*- lexical-binding: t; -*-

;;; Commentary: EMACS adapter to work with LLMs

;;; -*- lexical-binding: t; -*-

(require 'python)

;;; Code:

(defvar llm-python-process-number 0)
(defvar llm-python-process nil)

(defvar llm-python-file-name nil)
(defun llm-get-script-file-name ()
  "Load the python file."
  (interactive)
  (let* ((script-dir (or (and load-file-name (file-name-directory load-file-name))
                         (and buffer-file-name (file-name-directory buffer-file-name))
                         default-directory)))  ; Use 'default-directory' as a fallback
    (setq llm-python-file-name (concat script-dir "llm_emacs.py"))))

(defun llm-start-python ()
  "Start a dedicated Python process for LLM."
  (interactive)
  (llm-get-script-file-name)
  (run-python (concat "python3 -i " llm-python-file-name) llm-python-process-number t)
  (setq llm-python-process (get-process (python-shell-get-process-name llm-python-process-number)))
  
  ;; create response buffer
  (let ((response-buffer (get-buffer-create "*LLM Response*")))
    (with-current-buffer response-buffer
      (markdown-mode))  ; Ensure markdown-mode is installed
    (switch-to-buffer-other-window response-buffer))

  ;; Set up process filter (if needed)
  (set-process-filter llm-python-process 'llm-process-filter))

(defvar llm-filter-buffer "")
(defvar llm-in-stream-section nil "Flag to indicate if we are inside a streamed section.")

(defun llm-process-filter (_ string)
  "Custom process filter to handle LLM STRING output."
  (setq llm-filter-buffer (concat llm-filter-buffer string)) ; Append to buffer
  ;; Check and process only the relevant section
  (let ((start (string-match "<<LLM_START>>" llm-filter-buffer))
        (end (string-match "<<LLM_END>>" llm-filter-buffer)))
    (when (and start end)
      (let ((match (substring llm-filter-buffer (+ start (length "<<LLM_START>>")) end)))
        (with-current-buffer "*LLM Response*"
          (insert match))
        ;; Clear processed part from the buffer
        (setq llm-filter-buffer (substring llm-filter-buffer (+ end (length "<<LLM_END>>")))))
      (setq llm-in-stream-section nil)))) ; Reset flag

;; (defun llm-process-filter (_ string)
;;   (with-current-buffer "*LLM Response*"
;;     (insert string)))

;; Prompt Options
(defvar llm-options nil "JSON string of LLM options.")

(defun llm-set-options ()
  "Set the LLM options as a JSON string."
  (interactive)
  (setq llm-options (read-string "Enter LLM options (JSON format): "))
  (message "LLM options set: %s" llm-options))

;; System Prompt
(defvar llm-system-prompt nil "System prompt for LLM.")

(defun llm-set-system-prompt ()
  "Set the system prompt for LLM."
  (interactive)
  (setq llm-system-prompt (read-string "Enter LLM system prompt: "))
  (message "LLM system prompt set: %s" llm-system-prompt))

;; API
(defvar llm-selected-api nil "Currently selected API.")

(defun llm-set-api ()
  "Install an LLM plugin."
  (interactive)
    (let ((api (completing-read "Select an API: " (list "OpenAI" "Google"))))
      (setq llm-selected-api api)
      (message "Selected API: %s" api)))
  
;; Models
(defvar llm-models-list nil "List of available LLM models.")

(defun llm-get-models ()
  "Fetches and displays the list of available LLM models."
  (interactive)
  (let* ((script-dir (if load-file-name
                         (file-name-directory load-file-name)
                         (file-name-directory (buffer-file-name))))  ; Use 'default-directory' as a fallback
         (script-path (concat script-dir "models.py"))
         (models-json (shell-command-to-string (format "python3 '%s'" script-path))))
    (setq llm-models-list (json-parse-string models-json :array-type 'list))
    (message "Models loaded. Use llm-select-model to choose a model.")))

(defvar llm-selected-model nil "Currently selected LLM model.")

(defun llm-select-model ()
  "Allow the user to select a model from the available models."
  (interactive)
  (if (not llm-models-list)
      ;; (message "Model list is empty. Run llm-get-models first.")
	  (llm-get-models)
    (let ((model (completing-read "Select a model: " llm-models-list)))
      (setq llm-selected-model model)
      (message "Selected model: %s" model))))

;; Plugins
(defun llm-install-plugin (plugin-name)
  "Install an LLM plugin with PLUGIN-NAME."
  (interactive "sEnter the name of the plugin to install: ")
  (shell-command (format "llm install %s" plugin-name))
  (message "Plugin %s installation initiated." plugin-name))

(defun llm-uninstall-plugin (plugin-name)
  "Uninstall an LLM plugin with PLUGIN-NAME."
  (interactive "sEnter the name of the plugin to uninstall: ")
  (shell-command (format "llm uninstall %s -y" plugin-name))
  (message "Plugin %s uninstallation initiated." plugin-name))

(defun llm-list-plugins ()
  "List installed LLM plugins."
  (interactive)
  (shell-command "llm plugins")
  (switch-to-buffer "*Shell Command Output*")
  (message "Listing installed plugins."))

;; Prompting
(defun llm-run-prompt-shell-cmd (prompt)
  "Run a PROMPT through LLM with OPTIONS and a SYSTEM-PROMPT."
  (interactive "sEnter your LLM prompt: ")
  (if (not llm-selected-model)
      (message "No model selected. Use llm-select-model to choose a model.")
    (let* ((script-dir (if load-file-name
                           (file-name-directory load-file-name)
                           (file-name-directory (buffer-file-name))))
           (script-path (concat script-dir "llm_emacs.py"))
           (command (format "python3 '%s' '%s' '%s' '%s' '%s' '%s'"
							script-path llm-selected-api llm-selected-model prompt llm-options llm-system-prompt)))
	  (async-shell-command command "*LLM Stream Output*"))))

(defun llm-run-prompt (prompt)
  "Run a PROMPT through LLM with streaming output and capture the response."
  (interactive "sEnter your LLM prompt: ")
  (let ((command (format "run_llm('%s', '%s')\n" llm-selected-model prompt)))
    (process-send-string llm-python-process command)))

;; Conversation Mode
(defvar llm-conversation-state nil "Current state of the LLM conversation.")



;; Provide the package
(provide 'llm)
;;; llm.el ends here
