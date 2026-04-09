;;;; src/project-scaffold-templates.lisp
;;;;
;;;; Template string constants for project-scaffold. Kept in a dedicated
;;;; file so that bulk literal content does not clutter the logic modules.
;;;; All templates use {{name}}, {{description}}, {{author}}, {{license}},
;;;; {{parent-prompts}} placeholders resolved by render-template in
;;;; project-scaffold-core.

(defpackage #:cl-mcp/src/project-scaffold-templates
  (:use #:cl)
  (:export))

(in-package #:cl-mcp/src/project-scaffold-templates)
