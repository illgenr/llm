;;; llm.el --- Emacs porcelain for the program LLM  -*- lexical-binding: t; -*-

;;; Commentary: Work with LLMs in EMACS

;; This package utilizes the program LLM to make calls to various LLMs

;;;
;; Author: Raleigh Illgen
;; Maintainer: Raleigh Illgen
;; Version: 0.01
;; Package-Requires: (python)
;; Keywords: LLM

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

(defvar llm-current-chat-buffer nil)
(defun llm-new-chat ()
  "Create a new untitled chat."
  (interactive)
  (setq llm-current-chat-buffer "untitled.chat")
  (let ((response-buffer (get-buffer-create "untitled.chat")))
    (with-current-buffer response-buffer
	  (markdown-mode))  ; Ensure markdown-mode is installed
	(switch-to-buffer-other-window response-buffer)))

(defun llm-start ()
  "Start a dedicated Python process for LLM."
  (interactive)
  (llm-get-script-file-name)
  (run-python (concat "python3 -i " llm-python-file-name) 'project t)
  (setq llm-python-process (get-process (python-shell-get-process-name 'project)))
  (llm-new-chat))


(defvar llm-expecting-stream-output nil "Flag to indicate if we are inside a streamed section.")

(defun llm-preoutput-stream-handler (output)
  "Handle streamed OUTPUT from LLM."
  ;; (print output)
  (if llm-expecting-stream-output
      ;; Check for UTF-8 EOT character
      (if (string-match "\u0004" output)
          ;; Handle end of transmission
          (progn
			(setq llm-expecting-stream-output nil)
			(with-current-buffer llm-current-chat-buffer
			  (save-excursion
				(goto-char (point-max))))
            (with-current-buffer (process-buffer llm-python-process)
              (remove-hook 'comint-preoutput-filter-functions 'llm-preoutput-stream-handler))
            "")
        ;; Continue appending output
        (with-current-buffer llm-current-chat-buffer
          (save-excursion
            (goto-char (point-max))
            (insert output)))
        "")
    ;; If not expecting LLM output, just return the output
    output)
)


;; separate handler to create title to keep the code clear
(defvar llm-expecting-title nil "Flag to indicate we are receiving a title.")
(defvar llm-title-stream-buffer nil)

(defun llm-preoutput-title-stream-handler (output)
  "Handle title streamed OUTPUT from LLM."
  (if llm-expecting-title
      ;; Check for UTF-8 EOT character
	  (if (string-match "\u0004" output)
		  ;; Handle end of transmission
		  (progn
			(setq llm-expecting-title nil)
			(with-current-buffer (process-buffer llm-python-process)
			  (remove-hook 'comint-preoutput-filter-functions 'llm-preoutput-title-stream-handler))
			(with-current-buffer llm-current-chat-buffer
			  (progn
				(setq llm-title-stream-buffer (concat llm-title-stream-buffer ".chat"))
				(print llm-title-stream-buffer)
				(rename-buffer llm-title-stream-buffer)
				(markdown-mode)
				(setq llm-current-chat-buffer llm-title-stream-buffer)
				(setq llm-title-stream-buffer nil)))
			"")
		(progn
		  (setq llm-title-stream-buffer
				(concat llm-title-stream-buffer (replace-regexp-in-string "\n" "" output))))
		""
		)
	output)
) ; Always return the output if we're not expecting LLM output


(defvar llm-expecting-conversation-output nil "Flag to indicate if we are inside a streamed section.")

(defun llm-preoutput-conversation-handler (output)
  "Handle streamed OUTPUT from LLM."
  (if llm-expecting-conversation-output
      (let ((clean-output (if (string-match "\u0004" output)
                              (substring output 0 (match-beginning 0))
                            output)))
        (with-current-buffer llm-current-chat-buffer
          (save-excursion
            (goto-char (point-max))
            (insert clean-output)))
        (if (string-match "\u0004" output)
            (progn
              (setq llm-expecting-conversation-output nil)
			  (switch-to-buffer llm-current-chat-buffer)
              ""))
        ;; Return empty string to suppress further processing
        "")
    ;; Return original output if not processing conversation output
    output))

;; (defun llm-process-filter-open (string)
;;   (with-current-buffer "*LLM Response*"
;;     (insert string))
;;   string)
;; 
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
	  (let ((command (format "llmwrapper.set_model('%s')\n" model)))
		(process-send-string llm-python-process command))
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

;; Input Sanitizer
(defun sanitize-string (string)
  "Sanitize the STRING."
  ;; Escape backslashes first
  (setq string (replace-regexp-in-string "\\\\" "\\\\\\\\" string))
  ;; Escape other special characters
  (setq string (replace-regexp-in-string "\"" "\\\\\"" string))
  (setq string (replace-regexp-in-string "'" "\\\\'" string))
  (setq string (replace-regexp-in-string "\n" "\\\\n" string))
  (setq string (replace-regexp-in-string "\t" "\\\\t" string))
  string)

;; Prompting
(defun llm-run-prompt (prompt)
  "Run a PROMPT through LLM with streaming output and capture the response."
  (interactive "sEnter your LLM prompt: ")

  (let ((sanitized-prompt (sanitize-string prompt)))
	(print sanitized-prompt)
	(with-current-buffer (process-buffer llm-python-process)
      (add-hook 'comint-preoutput-filter-functions 'llm-preoutput-stream-handler))
	(setq llm-expecting-stream-output t)
	(with-current-buffer llm-current-chat-buffer
	  (goto-char (point-max)))
	(let ((command (format "llmwrapper.prompt(\"%s\")\n" sanitized-prompt)))
      (process-send-string llm-python-process command))))

;; Conversation Mode

;; Starting a Conversation
(defun llm-start-conversation ()
  "Start a conversation through LLM."
  (interactive)
  (process-send-string llm-python-process "llmwrapper.start_conversation()\n"))

;; Continuing a Conversation
(defun llm-continue-conversation (prompt)
  "Continue a conversation with LLM using PROMPT."
  (interactive "sEnter your LLM prompt for conversation: ")
  ;; Sanitize the prompt by escaping single and double quotes
  (let ((sanitized-prompt (sanitize-string prompt)))
    (with-current-buffer (process-buffer llm-python-process)
      (add-hook 'comint-preoutput-filter-functions 'llm-preoutput-stream-handler))
    (setq llm-expecting-stream-output t)
    (let ((command (format "llmwrapper.continue_conversation(\"%s\")\n" sanitized-prompt)))
      (process-send-string llm-python-process command))))

;; Ending a Conversation
(defun llm-end-conversation ()
  "End the current conversation with LLM."
  (interactive)
  (process-send-string llm-python-process "llmwrapper.end_conversation()\n"))

;; Title generator
(defvar llm-title-generator-prompt "Create a very short title for this chat to use as a filename no more than 20 characters. DO NOT use spaces, use underscores _ instead. Avoid quote characters:")

(defun llm-title-chat ()
  "Run a PROMPT through LLM with streaming output and capture the response."
  (interactive)
  (with-current-buffer (process-buffer llm-python-process)
    (add-hook 'comint-preoutput-filter-functions 'llm-preoutput-title-stream-handler))
  (setq llm-expecting-title t)
  (let ((buffer-content (with-current-buffer llm-current-chat-buffer
                          (let ((content (buffer-string)))
                            ;; Replace single quotes and newlines
                            (setq content (replace-regexp-in-string "'" "\\\\'" content))
                            (replace-regexp-in-string "\n" " " content)))))
    (let ((command (format "llmwrapper.prompt('%s')\n"
                           (concat llm-title-generator-prompt buffer-content))))
      (process-send-string llm-python-process command))))

;; Send region
(defun llm-send-region-as-prompt (start end)
  "Send a region (defined by START END) as a prompt."
  (interactive "r")
  (let ((region-text (format "%s" (buffer-substring-no-properties start end))))
    ; (message "Sending region as prompt: %s" region-text)
    (llm-run-prompt region-text)))

(defun llm-send-region-with-prompt (prompt)
  "Send a region as a PROMPT."
  (interactive "sBegin with prompt: ")
  (let ((region-text (format "%s \\n %s" prompt (buffer-substring-no-properties (region-beginning) (region-end)))))
    (llm-run-prompt region-text)))

(defun llm-send-region-conversation (start end)
  "Send a region (defined by START END) as a prompt."
  (interactive "r")
  (let ((region-text (format "%s" (buffer-substring-no-properties start end))))
    (llm-run-prompt region-text)))

(defun llm-send-region-conversation-with-prompt (prompt)
  "Send a region as a PROMPT."
  (interactive "sBegin with prompt: ")
  (llm-continue-conversation
   (format "%s \\n %s"
		   prompt
		   (buffer-substring-no-properties (region-beginning) (region-end)))))

(defvar llm-refactor-prompt "Refactor the following code following best practices as an expert programmer. Avoid explation. ")

(defun llm-refactor ()
  "Send a region to refactor to the existing conversation."
  (interactive)
  (llm-continue-conversation
   (format "%s \\n %s"
		   llm-refactor-prompt
		   (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun llm-send-buffer ()
  "Send a buffer as a prompt."
  (interactive)
  (let ((buffer-text (format "%s" (buffer-substring-no-properties 1 (point-max)))))
    (llm-continue-conversation buffer-text)))

;; Save and load
(defvar llm-chat-directory (expand-file-name "~/chat")
  "Default directory for saving and loading LLM chats.")

(defun llm-save-conversation (&optional file-name)
  "Save the current LLM conversation to a file in the default chat directory.
If FILE-NAME is not provided, use the name of llm-current-chat-buffer."
  (interactive)
  (let* ((buffer-name-to-use (if (bufferp llm-current-chat-buffer)
                                 (buffer-name llm-current-chat-buffer)
                                 llm-current-chat-buffer))
         (default-file-name (if file-name
                                file-name
                              (concat (replace-regexp-in-string "\\.chat$" "" buffer-name-to-use) ".chat"))))
    (unless (file-directory-p llm-chat-directory)
      (make-directory llm-chat-directory t))
    (let ((file-path (expand-file-name default-file-name llm-chat-directory)))
      (process-send-string llm-python-process (format "llmwrapper.save_conversation(\"%s\")\n"
                                                      (replace-regexp-in-string "'" "\\\\'" file-path))))))

(defvar llm-end-of-conversation-marker "\u0004"
  "Marker indicating the end of the conversation output from Python.")

(defun llm-preoutput-filter-for-conversation (output)
  "Preoutput filter to handle the conversation OUTPUT."
  (if (string-match llm-end-of-conversation-marker output)
      (progn
          (with-current-buffer (get-buffer llm-current-chat-buffer)
            (insert output)))
        ;; Remove this filter
        (remove-hook 'comint-preoutput-filter-functions 'llm-preoutput-filter-for-conversation)
        "")
    ;; Continue appending output to the buffer
    output)

(defun llm-load-conversation ()
  "Load an LLM conversation from a file using a file selection dialog."
  (interactive)
  ;; Use read-file-name to select the file
  (let ((file-name (read-file-name "Select conversation file: " llm-chat-directory nil t)))
    (with-current-buffer (get-buffer-create file-name))
    (setq llm-current-chat-buffer file-name)
	(with-current-buffer llm-current-chat-buffer
	  (markdown-mode))  ; Ensure markdown-mode is installed
    (with-current-buffer (process-buffer llm-python-process)
      (add-hook 'comint-preoutput-filter-functions 'llm-preoutput-conversation-handler))
    (setq llm-expecting-conversation-output t)
    (let ((file-path (expand-file-name file-name llm-chat-directory)))
      (process-send-string llm-python-process (format "llmwrapper.load_conversation('%s')\n" file-path)))))

;; Provide the package
(provide 'llm)
;;; llm.el ends here
