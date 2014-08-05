el-pocket
=========

el-pocket :: emacs -> getpocket.com

Installation/Setup/Usage:
-------------------------

Put this file in your load-path somewhere and require it. 
Do an M-x el-pocket-authorize. 
The first time you do this you will be directed to the oauth/request page, where you can click on authorize. 
After authorizing, return to emacs and M-x el-pocket-authorize again. 
This time you should get an access token, and it will be saved to "~/.el-pocket-auth.js". 
Once this is all done you should be able to el-pocket-add URLs ...
Now you can add these lines to your init file for future enuserating:
```
(require 'el-pocket)
(el-pocket-load-auth)
```

