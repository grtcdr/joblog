Joblog is a major mode and `completing-read`-based interface for a plain text file format that you can use to log your job applications.

If you're applying to a bunch of jobs on a bunch of different websites, it's very likely that you've run into the problem of staying sane as you attempt to keep a log of all your job applications.

Joblog's format is chronological, simple to read and write and most importantly, future-proof.

```
Ministry of Plenty: Accountant (1984-12-01) -- Oceania
Ministry of Truth: Chief Editor (1984-11-01) <Rejected> -- Oceania
Ministry of Peace: Lieutenant (1984-10-01) <Interviewed> -- Oceania
```

![Emacs frame showing joblog selection menu on one side and the text format on the other](media/preview.webp)

> I have yet to hear back from the Ministry of Plenty...

# Installation

Joblog is not yet available in any package archive so you'll have to obtain it
from source by evaluating the following:

``` emacs-lisp
(package-vc-install "https://git.sr.ht/~grtcdr/joblog" "2024_12_08.c311e1a")
```

# Usage

Your `joblog-file` is your personal database, you can use `customize-file` to set it. Type `M-x customize-group SPC joblog RET` to consult all of Joblog's customization options.

Call the interactive `joblog` function, it will prompt you to enter a few details such as the company, the job title, the location and the date you applied for the position.

The information you enter will be saved to `joblog-file`. If you hear back from that company, call `joblog-change-status`, select the job from the list of candidates and set the new status from the list of `joblog-status-list`, you're free to add your own items to the list.

The next time you use `joblog`, the `completing-read` interface will make suggestions based on the existing content of your `joblog-file` which could save you some typing time and keep your file consistent.

You can use `joblog-visit` to jump into your `joblog-file` and makes changes directly, for example, to delete an item or change the job title. It's just text, just be careful to respect the rules of the file format, see the next section for more information.

# Format

```
COMPANY: JOB TITLE (YYYY-MM-DD) [<STATUS> -- LOCATION]
# This is a comment :)
```

Notes:
- Status and location are noted in brackets to show that they are optional fields. When either of the fields is specified in the `completing-read` interface or during manual insertion, the brackets *must* not be included.
- Location can be used to insert the work modality, for example: remote, hybrid, etc.
- Comments can occupy one or more lines but they *should* not occupy the same line as a log entry, the reason is that font lock will break, but besides that, nothing stops you from doing this.

# Paradigm

Joblog's text-based nature opens the door to some interesting possibilities. For example, you can write Emacs Lisp functions or shell scripts that query your `joblog-file` for specific information and return pertinent statistics.

Here's an Emacs Lisp function that returns the rate of rejection of job applications:

``` emacs-lisp
(defun my-joblog-rejection-rate ()
  "Return the rate of rejection of job applications."
  (let* ((entries (joblog--entry-list (find-file-noselect joblog-file)))
         (rejected (seq-filter (lambda (s) (string-match-p "<Rejected>" s)) entries))
         (flength (lambda (list) (float (length list))))
         (rate (* 100.0 (/ (funcall flength rejected)
                           (funcall flength entries)))))
    (message (format "%.2f%%" rate))))
```

You can achieve similar results with `grep`, `wc` and other standard utilities.

Note that no such statistical functions are provided by the package, you are expected to write these yourself using the package's internal functions as illustrated in the previous example. Joblog is (and will likely remain) a tiny package making it possible to comprehend, modify and extend easily and quickly.

# Support

If you wish to contribute a patch, inquire about something or share your feedback, you are welcome to send an email to [~grtcdr/pub@lists.sr.ht](mailto:~grtcdr/pub@lists.sr.ht). If this is your first time collaborating over email, please check out [this guide](https://git-send-email.io/). If you encounter issues of any kind, please file them in the project's [ticket tracker][ticket-tracker].

[mailing-list]: mailto:~grtcdr/pub@lists.sr.ht
[ticket-tracker]: https://todo.sr.ht/~grtcdr/joblog
