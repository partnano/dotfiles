(custom-set-variables
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e"
     "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
     "329877489db257d1da4890cce8fc4ab16025a17151c18975a2a412aecfe7517d"
     "756af47141a4390fd57e9cb39f349d1e009b158b947a4ac05d98dc2c9d8249af"
     "2f076a213862a88945ff4f1bb88cc58a3dbd3a740e1ba769839a57e2f7c1d3a0"
     "7f14fad67d7ac8d0555bd5a1fd1a429ce2dd37162c4e2dc7ae501f54bad1273a"
     default))))

(push '"~/nano_config/themes" custom-theme-load-path)

(load-theme 'jazz t)
(set-face-attribute 'region nil :background "#1E253B")
(set-face-attribute 'default nil :foreground "#BBB")
