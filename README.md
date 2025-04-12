# Intro

Generate git commit messages on Magit using LLM providers, for example: OpenAI ChatGPT, Google Gemini, and more.

![melpa](https://melpa.org/packages/magit-gptcommit-badge.svg)

https://github.com/douo/magit-gptcommit/assets/743074/8494b235-aa1a-4404-82e3-0a73da86e833

# Dependencies

- [magit](https://magit.vc/)
- [llm](https://github.com/ahyatt/llm) or [gptel](https://github.com/karthink/gptel)
- curl

# Setup

`magit-gptcommit` requires either [llm](https://github.com/ahyatt/llm) or [gptel](https://github.com/karthink/gptel). One of these two packages needs to be installed and the customizable variable `magit-gptcommit-backend` needs to be correctly configured.

## llm
Please read the
[documentation](https://github.com/ahyatt/llm?tab=readme-ov-file#setting-up-providers)
of `llm` for more details on how to set up providers. Debug logging for `llm` can be enabled by setting `llm-log` to `t`.

For example, to set up the OpenAI provider, first create an [OpenAI API key](https://platform.openai.com/account/api-keys). Once obtained, configure the `llm` OpenAI provider with the API key (replace `"OPENAI-KEY"` with your key):

``` emacs-lisp
(setq magit-gptcommit-llm-provider (make-llm-openai :key "OPENAI-KEY"))
```

**Recommended**: See [below](#using-auth-source-for-api-keys) how to use Emacs `auth-source` to protect API keys for `llm` providers.

## gptel
Please read the
[documentation](https://github.com/karthink/gptel?tab=readme-ov-file#setup)
of gptel for more configuration details and provide the method to
configure OpenAI here.

Procure an [OpenAI API key](https://platform.openai.com/account/api-keys).

Setup a key:

- Recommend: Storing in ~/.authinfo. By default, “api.openai.com” is
  used as HOST and “apikey” as USER.

      machine api.openai.com login apikey password TOKEN

- Another: Set `gptel-api-key` to the key.

The backend used by `gptel` and model can be configured using `gptel-menu` before invoking magit-gptcommit related methods.

## Common magit-gptcommit setup

Activate `magit-gptcommit-mode` and open a Magit status buffer, the commit message will be automatically generated when changes are staged. You can run `magit-gptcommit-generate` when visiting a Magit status buffer to generate a commit message manually.

Setup example using [use-package](https://github.com/jwiegley/use-package) and [straight](https://github.com/radian-software/straight.el):

``` emacs-lisp
(use-package magit-gptcommit
  :straight t
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  ;; --- if using llm ---
  (magit-gptcommit-backend 'llm)
  (magit-gptcommit-llm-provider (make-llm-openai :key "OPENAI-KEY"))

  ;; --- if using gptel ---
  (magit-gptcommit-backend 'gptel)

  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
```

# Usage

| **Command**                      | Description                                                                             |
|----------------------------------|-----------------------------------------------------------------------------------------|
| `magit-gptcommit-generate`       | Generate gptcommit message and insert it into magit buffer.                             |
| `magit-gptcommit-commit-create`  | Execute \`magit-commit-create' and bring gptcommit message to editor.                   |
| `magit-gptcommit-commit-quick`   | Accept gptcommit message and make a commit with current staged.                         |
| `magit-gptcommit-commit-accept`  | Call on `COMMIT_EDITMSG` buffer, Accept gptcommit message after saving current message. |
| `magit-gptcommit-abort`          | Abort the query process of current Repository.                                          |
| `magit-gptcommit-remove-section` | Remove the gptcommit section from the current magit buffer.                             |

| **Variable**                               | Description                                                                                  |
|--------------------------------------------|----------------------------------------------------------------------------------------------|
| `magit-gptcommit-backend`                  | The backend used by `magit-gptcommit`. Should be either `llm` or `gptel`                     |
| `magit-gptcommit-prompt`                   | Prompt.                                                                                      |
| `magit-gptcommit-max-token`                | Default 4096, magit-gptcommit will truncate excessive characters based on 1 token = 4 chars  |
| `magit-gptcommit-determine-max-token`      | Whether to use the llm provider max tokens, used only if `magit-gptcommit-max-token` is nil. |
| `magit-gptcommit-cache-limit`              | Cache size, default is 30                                                                    |
| `magit-gptcommit--cache`                   | Cache of last generated commit message.                                                      |
| `magit-gptcommit-llm-provider`             | llm provider or a function that returns an llm provider.                                     |
| `magit-gptcommit-llm-provider-temperature` | llm provider temperature. (float)                                                            |
| `magit-gptcommit-llm-provider-max-tokens`  | llm provider max generated tokens. (integer)                                                 |

# Using `auth-source` for API keys

Setup an `llm` provider for Google Gemini using [use-package](https://github.com/jwiegley/use-package), [straight](https://github.com/radian-software/straight.el), and Emacs `auth-source`, given that there is an `auth-source` secret stored with the host "generativelanguage.googleapis.com" and the user "apikey".

``` emacs-lisp
;; --- if using llm ---
(use-package llm
  :straight t
  :init
  (require 'llm-gemini)
  :config
  (setopt llm-gemini-provider
          (make-llm-gemini :key (auth-info-password
                                 (car (auth-source-search
                                       :host "generativelanguage.googleapis.com"
                                       :user "apikey")))
                           :chat-model "gemini-1.5-flash-latest"))
  :custom
  (llm-warn-on-nonfree nil))

;; --- if using gptel ---
;; TODO

;; --- magit-gptcommit config ---
(use-package magit-gptcommit
  :straight t
  :demand t
  :custom
  (magit-gptcommit-llm-provider llm-gemini-provider)
...)
```

Content of the Auth Sources(`~/.authinfo`):
```
machine generativelanguage.googleapis.com login apikey password <api-key>
```
> [!TIP]
> Mastering Emacs has a great article on how to store secrets in Emacs using GnuPG and Auth Sources: [Keeping Secrets in Emacs](https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources)

# Todo

- [ ] Prompt optimization
  - There is still a lot of room for optimization. One problem is the context limitation, for example, GPT3.5 has a 4k context limit, so making a slightly larger edit will exceed the limit. Currently, the code roughly truncates the edit based on proportions to avoid this problem.
  - Alternatively, we could use a method similar to [gptcommit](https://github.com/zurawiki/gptcommit). This method summarizes the diff of each file and then merges all the summaries to generate the commit message. However, this approach requires n + 2 requests to generate a single message.
- [ ] LRU Cache or Disk Cache
- [X] NO Stream
- [X] Other LLM Support

# Credit

- [llm](https://github.com/ahyatt/llm) Great project.
- [gptel](https://github.com/karthink/gptel) Great project.
- [gptcommit](https://github.com/zurawiki/gptcommit) Very helpful, Prompt modified from this project.
- [magit-todos](https://github.com/alphapapa/magit-todos) This project has been of great help to me in learning magit development.
- [GPT-Commit](https://github.com/ywkim/gpt-commit): Another Emacs pacakge for a similar purpose.
