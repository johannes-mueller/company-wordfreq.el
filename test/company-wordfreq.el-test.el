;;; company-wordfreq.el-test.el --- Tests for company-wordfreq

;;; Commentary:

;; usually run by ert-runner

;;; Code:

(require 'mocker)
(require 'company-wordfreq)

(ert-deftest test-candidates-foo ()
  (let ((company-wordfreq--grep-executable "/path/to/grep-program"))
    (mocker-let ((shell-command-to-string (command)
					  ((:input
					    '("/path/to/grep-program -i \\^foo /path/to/dict.txt")
					   :output "foobar\nfoobaz\nfoo")))
		(company-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
     (should (equal (company-wordfreq--candidates "foo") '("foobar" "foobaz" "foo"))))))

(ert-deftest test-candidates-foo-case-sensitive ()
  (let ((company-wordfreq--grep-executable "/path/to/grep-program"))
    (mocker-let ((shell-command-to-string (command)
					  ((:input
					    '("/path/to/grep-program -i \\^Foo /path/to/dict.txt")
					   :output "foobar\nfoobaz\nfoo")))
		(company-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
     (should (equal (company-wordfreq--candidates "Foo") '("foobar" "foobaz" "foo"))))))

(ert-deftest test-candidates-bar ()
  (let ((company-wordfreq--grep-executable "/other/path/to/grep-program"))
    (mocker-let ((shell-command-to-string (command)
					  ((:input
					    '("/other/path/to/grep-program -i \\^bar /other/path/to/dict.txt")
					    :output "barbar\nbarbaz\nbar")))
		(company-wordfreq--dictionary () ((:output "/other/path/to/dict.txt"))))
     (should (equal (company-wordfreq--candidates "bar") '("barbar" "barbaz" "bar"))))))

(ert-deftest test-dict-path-esperanto ()
  (let ((ispell-local-dictionary "esperanto")
	(company-wordfreq-path "/path/to/dicts"))
    (should (equal (company-wordfreq--dictionary) "/path/to/dicts/esperanto.txt"))))

(ert-deftest test-dict-path-english ()
  (let ((ispell-local-dictionary "english")
	(company-wordfreq-path "/other/path/to/dicts"))
    (should (equal (company-wordfreq--dictionary) "/other/path/to/dicts/english.txt"))))

(ert-deftest test-find-grep-program ()
  (let ((company-wordfreq--grep-executable nil))
    (mocker-let ((executable-find (program) ((:input '("grep") :output "/usr/bin/grep"))))
     (company-wordfreq--find-grep-program)
     (should (equal company-wordfreq--grep-executable "/usr/bin/grep")))))

(ert-deftest test-find-grep-program-not-avail ()
  (let ((company-wordfreq--grep-executable nil))
    (mocker-let ((executable-find (program) ((:input '("grep") :output nil))))
      (should-error (company-wordfreq--find-grep-program)))))

(ert-deftest test-backend-init ()
  (mocker-let ((company-wordfreq--find-grep-program () ((:occur 1))))
    (company-wordfreq 'init)))

(ert-deftest test-backend-prefix-foo ()
  (mocker-let ((company-grab-word () ((:output "foo"))))
    (should (equal (company-wordfreq 'prefix) "foo"))))

(ert-deftest test-backend-prefix-bar ()
  (mocker-let ((company-grab-word () ((:output "bar"))))
    (should (equal (company-wordfreq 'prefix) "bar"))))

(ert-deftest test-backend-prefix-nil ()
  (mocker-let ((company-grab-word () ((:output nil))))
    (should (equal (company-wordfreq 'prefix) nil))))

(ert-deftest test-backend-candidates-foo ()
  (mocker-let ((company-wordfreq--candidates (prefix) ((:input '("foo") :output '("foo" "foobar" "foobaz"))))
	       (company-dabbrev (cmd arg) ((:input '(candidates "foo") :output nil))))
    (should (equal (company-wordfreq 'candidates "foo") '("foo" "foobar" "foobaz")))))

(ert-deftest test-backend-candidates-bar ()
  (mocker-let ((company-wordfreq--candidates (prefix) ((:input '("bar") :output '("bar" "barbar" "barbaz"))))
	       (company-dabbrev (cmd arg) ((:input '(candidates "bar") :output nil))))
    (should (equal (company-wordfreq 'candidates "bar") '("bar" "barbar" "barbaz")))))

(ert-deftest test-backend-candidates-bar-deduplicate ()
  (mocker-let
      ((company-wordfreq--candidates (prefix) ((:input '("bar") :output '("bar" "barbar" "bar" "barbaz"))))
       (company-dabbrev (cmd arg) ((:input '(candidates "bar") :output nil))))
    (should (equal (company-wordfreq 'candidates "bar") '("bar" "barbar" "barbaz")))))

(ert-deftest test-backend-sorted ()
  (should (eq (company-wordfreq 'sorted) 't)))

(ert-deftest test-backend-ignore-case ()
  (should (eq (company-wordfreq 'ignore-case) 'keep-prefix)))

(ert-deftest test-backend-duplicates ()
  (should (eq (company-wordfreq 'duplicates) nil)))

(ert-deftest test-wordfreq-path-default ()
  (let ((user-emacs-directory "/home/user/.emacs.d"))
    (should (equal (company-wordfreq--default-path) "/home/user/.emacs.d/wordfreq-dicts"))))

(ert-deftest test-language-proposal-list ()
  (let ((company-wordfreq--language-alist '(("esperanto" . "eo")
					    ("english" . "en"))))
    (should (equal (company-wordfreq--proposal-list) '("esperanto" "english")))))

(ert-deftest test-language-iso-code-list ()
  (let ((company-wordfreq--language-alist '(("esperanto" . "eo")
					    ("english" . "en"))))
    (should (equal (company-wordfreq--iso-code "esperanto") "eo"))))

(ert-deftest test-make-url-full ()
  (should (equal (company-wordfreq--dict-url "eo" "full")
		 "https://raw.githubusercontent.com/johannes-mueller/FrequencyWords/master/content/2018/eo/eo_full.txt")))

(ert-deftest test-make-url-50k ()
  (should (equal (company-wordfreq--dict-url "eo" "50k")
		 "https://raw.githubusercontent.com/johannes-mueller/FrequencyWords/master/content/2018/eo/eo_50k.txt")))

(defun return-code (code)
  (let ((buffer (generate-new-buffer "urltmp")))
    (with-current-buffer buffer
      (insert (concat "HTTP/1.1 " code "\nother headers")))
    buffer))

(ert-deftest test-probe-50k-word-list-true ()
  (mocker-let
      ((url-retrieve-synchronously (url inhibit-cookies)
				   ((:input
				     '("http://example.com/eo_50k.txt" :inhibit-cookies)
				     :output (return-code "200 OK"))))
       (company-wordfreq--dict-url (lang-code kind)
				   ((:input '("eo" "50k") :output "http://example.com/eo_50k.txt"))))
    (should (company-wordfreq--probe-50k "eo"))))

(ert-deftest test-probe-50k-word-list-false ()
  (mocker-let
      ((url-retrieve-synchronously (url inhibit-cookies)
				   ((:input
				     '("http://example.com/eo_50k.txt" :inhibit-cookies)
				     :output (return-code "404 Not Found"))))
       (company-wordfreq--dict-url (lang-code kind)
				   ((:input '("eo" "50k") :output "http://example.com/eo_50k.txt"))))
    (should-not (company-wordfreq--probe-50k "eo"))))

(ert-deftest test-drop-frequency-values ()
  (with-temp-buffer
    (insert "mi 16311
vi 13927
ne 11163
estas 10726
")
    (company-wordfreq--drop-frequency-values)
    (should (equal (buffer-string) "mi\nvi\nne\nestas\n"))))

(ert-deftest test-fetch-short ()
  (mocker-let ((y-or-n-p (str) ((:input '("Use reduced length 50k words? ")))))
    (company-wordfreq--prompt-fetch-short)))

(ert-deftest test-download-new-word-list-no50k ()
  (let ((company-wordfreq--word-list-buffer nil))
    (mocker-let ((company-wordfreq--proposal-list () ((:output '("esperanto" "english"))))
		(completing-read (prompt choices)
				 ((:input
				   '("Choose language: " ("esperanto" "english"))
				   :output "esperanto")))
		(company-wordfreq--iso-code (language) ((:input '("esperanto") :output "eo")))
		(company-wordfreq--probe-50k (lang-code) ((:input '("eo") :output nil)))
		(company-wordfreq--prompt-fetch-short () ((:occur 0)))
		(company-wordfreq--dict-url (lang-code kind)
					    ((:input '("eo" "full") :output "http://example.com/eo_full.txt")))
		(url-retrieve (url callback language inhibit-cookies)
			      ((:input
				'("http://example.com/eo_full.txt"
				  company-wordfreq--list-retrieved-callback
				  ("esperanto")
				  :inhibit-cookies)
				:output 'buffer))))
     (company-wordfreq-download-list)
     (should (eq company-wordfreq--word-list-buffer 'buffer)))))

(ert-deftest test-download-new-word-list-50k-no ()
  (let ((company-wordfreq--word-list-buffer nil))
    (mocker-let ((company-wordfreq--proposal-list () ((:output '("esperanto" "english"))))
		(completing-read (prompt choices)
				 ((:input
				   '("Choose language: " ("esperanto" "english"))
				   :output "esperanto")))
		(company-wordfreq--iso-code (language) ((:input '("esperanto") :output "eo")))
		(company-wordfreq--probe-50k (lang-code) ((:input '("eo") :output t)))
		(company-wordfreq--prompt-fetch-short () ((:output nil)))
		(company-wordfreq--dict-url (lang-code kind)
					    ((:input '("eo" "full") :output "http://example.com/eo_full.txt")))
		(url-retrieve (url callback language inhibit-cookies)
			      ((:input
				'("http://example.com/eo_full.txt"
				  company-wordfreq--list-retrieved-callback
				  ("esperanto")
				  :inhibit-cookies)
				:output 'buffer))))
     (company-wordfreq-download-list)
     (should (eq company-wordfreq--word-list-buffer 'buffer)))))

(ert-deftest test-download-new-word-list-50k-yes ()
  (let ((company-wordfreq--word-list-buffer nil))
    (mocker-let ((company-wordfreq--proposal-list () ((:output '("esperanto" "english"))))
		(completing-read (prompt choices)
				 ((:input
				   '("Choose language: " ("esperanto" "english"))
				   :output "esperanto")))
		(company-wordfreq--iso-code (language) ((:input '("esperanto") :output "eo")))
		(company-wordfreq--probe-50k (lang-code) ((:input '("eo") :output t)))
		(company-wordfreq--prompt-fetch-short () ((:output t)))
		(company-wordfreq--dict-url (lang-code kind)
					    ((:input '("eo" "50k") :output "http://example.com/eo_50k.txt")))
		(url-retrieve (url callback language inhibit-cookies)
			      ((:input '("http://example.com/eo_50k.txt"
					 company-wordfreq--list-retrieved-callback
					 ("esperanto")
					 :inhibit-cookies)
				       :output 'buffer))))
     (company-wordfreq-download-list)
     (should (eq company-wordfreq--word-list-buffer 'buffer)))))

(ert-deftest test-drop-http-response-header ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK
other headers

mi 16311
vi 13927
ne 11163
estas 10726
")
    (company-wordfreq--drop-http-response-header)
    (should (equal (buffer-string) "mi 16311\nvi 13927\nne 11163\nestas 10726\n"))))

(ert-deftest test-list-retrieved-callback-success ()
  (let ((company-wordfreq--word-list-buffer (generate-new-buffer "word-list-test-buffer"))
	(company-wordfreq-path (concat (file-name-directory (temporary-file-directory))
				       (make-temp-name ".emacs.d")))
	(buffer-tmp nil))
    (with-current-buffer company-wordfreq--word-list-buffer
      (insert "HTTP/1.1 200 OK
other headers

mi 16311
vi 13927
ne 11163
estas 10726
"))
    (setq buffer-tmp company-wordfreq--word-list-buffer)
    (company-wordfreq--list-retrieved-callback '(:peer 'foo) "esperanto")
    (should (equal (with-temp-buffer
		     (insert-file-contents (concat (file-name-as-directory company-wordfreq-path)
						   "esperanto.txt"))
		     (buffer-string)) "mi\nvi\nne\nestas\n"))
    (should-error (switch-to-buffer buffer-tmp))
    (should (eq company-wordfreq--word-list-buffer nil))))

(ert-deftest test-list-retrieved-callback-error ()
  (mocker-let ((company-wordfreq--drop-frequency-values () ((:occur 0))))
    (should-error (company-wordfreq--list-retrieved-callback '(:error 'foo) '("esperanto")))))


;;; company-wordfreq.el-test.el ends here
