" $ cp a0.vim ~/.vim/syntax/a0.vim
" $ grep '.a0' ~/.vimrc
" autocmd BufNewFile,BufRead *.a0 setlocal filetype=a0

if exists("b:current_syntax")
    finish
endif

syn match Comment   "#.*$"
syn match Operator  "[(){}\.;:=\\>+\-*/]"
syn match Number    "\<[0-9]\+\>"

syn region String start=+"+ skip=+\\"+ end=+"+

" NOTE: See `http://vimdoc.sourceforge.net/htmldoc/syntax.html`.
syn keyword Conditional
    \ if
    \ then
    \ else
    \ return

let b:current_syntax = "a0"
