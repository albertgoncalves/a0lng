" $ cp a0.vim ~/.vim/syntax/a0.vim
" $ grep '.a0' ~/.vimrc
" autocmd BufNewFile,BufRead *.a0 setlocal filetype=a0

if exists("b:current_syntax")
    finish
endif

syn match Comment  "#.*$"
syn match Operator "[(){}\.;:=\\>+\-*/%]"
syn match Number   "\<[0-9]\+\>"

syn match a0Special   contained "\\\(n\|\"\|\\\)"

syn region String start=+"+ skip=+\\"+ end=+"+ contains=a0Special

hi def link a0Special SpecialChar

" NOTE: See `http://vimdoc.sourceforge.net/htmldoc/syntax.html`.
syn keyword Statement
    \ if
    \ then
    \ else
    \ return
syn keyword Keyword
    \ print
    \ toString

let b:current_syntax = "a0"
