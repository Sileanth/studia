" Vim systax file
" Language:  helium

if exists("b:current_syntax")
	finish
endif

syn case match

syn keyword heTodo contained TODO FIXME XXX NOTE
syn region  heCommentL start="//" end="$" contains=heTodo
syn region  heComment start="(\*" end="\*)" contains=heComment,heCommentL,heTodo

syn match heParenErr ")"
syn match heCommentErr "*)"

syn region heParen transparent start="(" end=")" contains=ALLBUT,heParenErr

syn match heEscapeErr "\\\([^nbtr\\'\"\n]\)" contained

syn keyword heKeyword and begin data effect effrow elif else end extern
syn keyword heKeyword fn functor handle if in
syn keyword heKeyword include let match module pragma of
syn keyword heKeyword open rec return sig struct then this type val with
syn match   heNumber "[1-9][0-9]*" contained
syn match   heNumber "0[bB][01]+" contained
syn match   heNumber "0[oO][0-7]+" contained
syn match   heNumber "0[0-7]*" contained
syn match   heNumber "0[xX][0-9a-fA-F]+" contained
syn match   heNumberLit "[0-9][a-zA-Z0-9_]*" contains=heNumber
syn match   heVariable "\<[a-z_][0-9a-zA-Z_']*\>" contains=heKeyword
syn match   heType "\<[A-Z][0-9a-zA-Z_']*\>"
syn match   heSpecial "[]{}[|:,.;]\|->\|=>\?\|\(//\@!\)"
syn match   heCharLit "'\([^\\\n]\|\\\(['\"\\ntbr]\)\)'"
syn match   heStringLit "\"\([^\\\n\"]\|\\\(.\)\)*\(\"\|$\)" contains=heEscapeErr

let b:current_syntax = "helium"

hi def link heTodo         Todo
hi def link heComment      Comment
hi def link heCommentL     Comment
hi def link heKeyword      Keyword
hi def link heSpecial      Keyword
hi def link heParen        Keyword
hi def link heNumber       Number
hi def link heCharLit      Character
hi def link heStringLit    String
hi def link neVariable     Identifier
hi def link heType         Type

hi def link heNumberLit    Error
hi def link heParenErr     Error
hi def link heCommentErr   Error
hi def link heEscapeErr    Error
