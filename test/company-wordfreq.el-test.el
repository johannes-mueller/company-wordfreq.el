;;; company-wordfreq.el-test.el --- Tests for company-wordfreq

;;; Commentary:

;; usually run by ert-runner

;;; Code:

(require 'mocker)
(require 'company-wordfreq)

(ert-deftest test-rg-candidates-foo ()
  (mocker-let ((executable-find (executable) ((:input '("rg") :output "/path/to/rg")))
	       (shell-command-to-string (command)
					((:input
					  '("/path/to/rg -i -N ^foo /path/to/dict.txt")
					  :output "foobar\nfoobaz\nfoo")))
	       (company-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
      (should (equal (company-wordfreq--candidates "foo") '("foobar" "foobaz" "foo")))))

(ert-deftest test-rg-candidates-foo-case-sensitive ()
  (mocker-let ((executable-find (executable) ((:input '("rg") :output "/path/to/rg")))
	       (shell-command-to-string (command)
					((:input
					  '("/path/to/rg -i -N ^Foo /path/to/dict.txt")
					  :output "foobar\nfoobaz\nfoo")))
	       (company-wordfreq--dictionary () ((:output "/path/to/dict.txt"))))
      (should (equal (company-wordfreq--candidates "Foo") '("foobar" "foobaz" "foo")))))

(ert-deftest test-rg-candidates-bar ()
  (mocker-let ((executable-find (executable) ((:input '("rg") :output "/other/path/to/rg")))
	       (shell-command-to-string (command)
					((:input
					  '("/other/path/to/rg -i -N ^bar /other/path/to/dict.txt")
					  :output "barbar\nbarbaz\nbar")))
	       (company-wordfreq--dictionary () ((:output "/other/path/to/dict.txt"))))
      (should (equal (company-wordfreq--candidates "bar") '("barbar" "barbaz" "bar")))))

(ert-deftest test-rg-dict-path-esperanto ()
  (let ((ispell-local-dictionary "esperanto")
	(company-wordfreq-path "/path/to/dicts"))
    (should (equal (company-wordfreq--dictionary) "/path/to/dicts/esperanto.txt"))))

(ert-deftest test-rg-dict-path-english ()
  (let ((ispell-local-dictionary "english")
	(company-wordfreq-path "/other/path/to/dicts"))
    (should (equal (company-wordfreq--dictionary) "/other/path/to/dicts/english.txt"))))

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
  (mocker-let ((company-wordfreq--candidates (prefix) ((:input '("bar") :output '("bar" "barbar" "bar" "barbaz"))))
	       (company-dabbrev (cmd arg) ((:input '(candidates "bar") :output nil))))
    (should (equal (company-wordfreq 'candidates "bar") '("bar" "barbar" "barbaz")))))

(ert-deftest test-backend-sorted ()
  (should (eq (company-wordfreq 'sorted) 't)))

(ert-deftest test-backend-ignore-case ()
  (should (eq (company-wordfreq 'ignore-case) 'keep-prefix)))

(ert-deftest test-backend-duplicates ()
  (should (eq (company-wordfreq 'duplicates) nil)))

;;; company-wordfreq.el-test.el ends here
