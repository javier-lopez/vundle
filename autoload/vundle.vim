" Vundle    Is a simple plugin manager for Vim, this is a merge revision
"           between the original vundle and plug
" Author:   gmarik & junegunn
" HomePage: http://github.com/chilicuil/vundle

"TODO 12-12-2014 20:11 >> comment code
"TODO 12-12-2014 19:42 >> support hash bundle versions

if exists('g:loaded_vundle')
  finish
endif
let g:loaded_vundle = 1

let s:cpo_save = &cpo
set cpo&vim

let s:vundle_tab = get(s:, 'vundle_tab', -1)
let s:vundle_buf = get(s:, 'vundle_buf', -1)
let s:is_unix    = has('unix')
let s:is_win     = has('win32') || has('win64')
let s:mac_gui    = has('gui_macvim') && has('gui_running')
let s:nvim       = has('nvim') && exists('*jobwait') && !s:is_win
let s:base_spec  = { 'branch': 'master', 'frozen': 0 }
let s:loaded     = get(s:, 'loaded', {})
let s:triggers   = get(s:, 'triggers', {})
let s:TYPE       = {
\   'string':  type(''),
\   'list':    type([]),
\   'dict':    type({}),
\   'funcref': type(function('call'))
\ }
let s:vundle_interactive = 1

function! vundle#rc(...)
  if exists('s:vundle_rc_init') | return | endif
  let s:vundle_rc_init = 1
  if !executable('git') | return s:err('`git` executable not found. Vundle requires git.') | endif
  if a:0 > 0
    let s:vundle_home_org = a:1
    let home = s:path(fnamemodify(expand(a:1), ':p'))
  elseif exists('g:vundle_home')
    let home = s:path(g:vundle_home)
  elseif !empty(&rtp)
    let home = s:path(split(&rtp, ',')[0]) . '/bundle'
  else
    return s:err('Unable to determine vundle home. Try calling vundle#rc() with a path argument.')
  endif

  let g:vundle_home   = home
  let g:bundles       = {}
  let g:bundles_order = []
  let s:triggers      = {}

  call s:define_commands()
  call s:load_vimrc()
  call vundle#end()
  "return 1
endfunction

function! s:define_commands()
  command! -nargs=+ -bar Bundle call s:add(<args>)
  command! -nargs=* -bar -bang -complete=customlist,s:names BundleInstall call s:install('<bang>' == '!', [<f-args>])
  command! -nargs=* -bar -bang -complete=customlist,s:names BundleUpdate  call s:update('<bang>' == '!', [<f-args>])
  command! -nargs=0 -bar -bang BundleClean call s:clean('<bang>' == '!')
  command! Bundleclean   BundleClean
  command! -nargs=0 -bar BundleStatus   call s:status()
  command! BundleList    BundleStatus
  command! Bundlelist    BundleStatus
  command! -nargs=0 -bar BundleDiff     call s:diff()
  command! -nargs=? -bar BundleSnapshot call s:snapshot(<f-args>)
endfunction

function! s:load_vimrc()
  "this function deprecates vundle#end() into the user vimrc

  "vim was called specifying a custom vimrc file
  if $MYVIMRC == ""
      if s:is_unix
          let l:vim_argv = split(system('tr "\0" " " </proc/' . getpid() . '/cmdline'))
          for l:arg in l:vim_argv
              if exists("l:user_vimrc_found")
                  let l:user_vimrc = l:arg
                  break
              endif
              if (l:arg == '-Nu' || l:arg == '-NU' || l:arg == '-u' || l:arg == '-U')
                  let l:user_vimrc_found = 1
              endif
          endfor
          if exists("l:user_vimrc") | let $MYVIMRC = l:user_vimrc | endif
      endif
      if $MYVIMRC == "" | let $MYVIMRC=expand("~/.vimrc") | endif
      unlet s:vundle_interactive
  endif

  for l:line in readfile($MYVIMRC)
      "read everything but commented bundle lines
    if l:line =~ '^\s*Bundle.*' | execute l:line | endif
  endfor
  command! -nargs=+ -bar Bundle
endf

function! vundle#end()
  if !exists('g:bundles')
    return s:err('Call vundle#rc() first')
  endif

  if exists('#VundleLOD')
    augroup VundleLOD autocmd!
    augroup END
    augroup! VundleLOD
  endif
  let lod = {}

  filetype off
  for name in g:bundles_order
    let bundle = g:bundles[name]
    if get(s:loaded, name, 0) || !has_key(bundle, 'on') && !has_key(bundle, 'for')
      let s:loaded[name] = 1
      continue
    endif

    if has_key(bundle, 'on')
      let s:triggers[name] = { 'map': [], 'cmd': [] }
      for cmd in s:to_a(bundle.on)
        if cmd =~ '^insert'
          if !exists("l:vundle_insert_lod") | let l:vundle_insert_lod=[] | endif
          call add(l:vundle_insert_lod,name)
        elseif cmd =~ '^delay'
          if !exists("l:vundle_delay_lod") | let l:vundle_delay_lod=[] | endif
          call add(l:vundle_delay_lod,name)
        elseif cmd =~ '^<Plug>.\+'
          if empty(mapcheck(cmd)) && empty(mapcheck(cmd, 'i'))
            for [mode, map_prefix, key_prefix] in
                  \ [['i', '<C-O>', ''], ['n', '', ''], ['v', '', 'gv'], ['o', '', '']]
              execute printf(
              \ '%snoremap <silent> %s %s:<C-U>call <SID>lod_map(%s, %s, "%s")<CR>',
              \ mode, cmd, map_prefix, string(cmd), string(name), key_prefix)
            endfor
          endif
          call add(s:triggers[name].map, cmd)
        elseif cmd =~ '^[A-Z]'
          if exists(':'.cmd) != 2
            execute printf(
            \ 'command! -nargs=* -range -bang %s call s:lod_cmd(%s, "<bang>", <line1>, <line2>, <q-args>, %s)',
            \ cmd, string(cmd), string(name))
          endif
          call add(s:triggers[name].cmd, cmd)
        endif
      endfor
    endif

    if has_key(bundle, 'for')
      let types = s:to_a(bundle.for)
      if !empty(types)
        call s:source(s:rtp(bundle), 'ftdetect/**/*.vim', 'after/ftdetect/**/*.vim')
      endif
      for key in types
        if !has_key(lod, key)
          let lod[key] = []
        endif
        call add(lod[key], name)
      endfor
    endif
  endfor

  for [key, names] in items(lod)
    augroup VundleLOD
      execute printf('autocmd FileType %s call <SID>lod_ft(%s, %s)',
            \ key, string(key), string(names))
    augroup END
  endfor

  if exists("l:vundle_insert_lod")
    augroup VundleInsertLOD
    autocmd!
    for name in l:vundle_insert_lod
      execute "autocmd InsertEnter * call vundle#load(" . string(name) . ") | autocmd! VundleInsertLOD"
    endfor
    augroup END
  endif

  if exists("l:vundle_delay_lod")
    augroup VundleDelayLOD
    autocmd!
    for name in l:vundle_delay_lod
      for cmd in s:to_a(g:bundles[name].on)
        if cmd =~ "^delay"
          let delay=get(split(cmd, " "), 1, "10")
          if delay !~ '[0-9]' | let delay=10 | endif
          break
        endif
      endfor
      "echo string(name) . " " . delay
      execute "autocmd CursorHold,CursorMoved * call vundle#loadafter(" . string(name) . ", " . delay . ")"
    endfor
    augroup END
  endif

  call s:reorg_rtp()
  filetype plugin indent on
  if has('vim_starting')
    syntax enable
  else
    call s:reload()
  endif
endfunction

function! s:to_a(v)
  return type(a:v) == s:TYPE.list ? a:v : [a:v]
endfunction

function! s:to_s(v)
  return type(a:v) == s:TYPE.string ? a:v : join(a:v, "\n") . "\n"
endfunction

function! s:source(from, ...)
  for pattern in a:000
    for vim in s:lines(globpath(a:from, pattern))
      execute 'source' vim
    endfor
  endfor
endfunction

function! s:loaded_names()
  return filter(copy(g:bundles_order), 'get(s:loaded, v:val, 0)')
endfunction

function! s:reload()
  for name in s:loaded_names()
    call s:source(s:rtp(g:bundles[name]), 'plugin/**/*.vim', 'after/plugin/**/*.vim')
  endfor
endfunction

function! s:trim(str)
  return substitute(a:str, '[\/]\+$', '', '')
endfunction

if s:is_win
  function! s:rtp(spec)
    return s:path(a:spec.dir . get(a:spec, 'rtp', ''))
  endfunction

  function! s:path(path)
    return s:trim(substitute(a:path, '/', '\', 'g'))
  endfunction

  function! s:dirpath(path)
    return s:path(a:path) . '\'
  endfunction

  function! s:is_local_bundle(repo)
    return a:repo =~? '^[a-z]:'
  endfunction
else
  function! s:rtp(spec)
    return s:dirpath(a:spec.dir . get(a:spec, 'rtp', ''))
  endfunction

  function! s:path(path)
    return s:trim(a:path)
  endfunction

  function! s:dirpath(path)
    return substitute(a:path, '[/\\]*$', '/', '')
  endfunction

  function! s:is_local_bundle(repo)
    return a:repo[0] =~ '[/$~]'
  endfunction
endif

function! s:err(msg)
  echohl ErrorMsg
  echom a:msg
  echohl None
  return 0
endfunction

function! s:esc(path)
  return escape(a:path, ' ')
endfunction

function! s:escrtp(path)
  return escape(a:path, ' ,')
endfunction

function! s:remove_rtp()
  for name in s:loaded_names()
    let rtp = s:rtp(g:bundles[name])
    execute 'set rtp-='.s:escrtp(rtp)
    let after = globpath(rtp, 'after')
    if isdirectory(after)
      execute 'set rtp-='.s:escrtp(after)
    endif
  endfor
endfunction

function! s:reorg_rtp()
  if !empty(s:first_rtp)
    execute 'set rtp-='.s:first_rtp
    execute 'set rtp-='.s:last_rtp
  endif

  " &rtp is modified from outside
  if exists('s:prtp') && s:prtp !=# &rtp
    call s:remove_rtp()
    unlet! s:middle
  endif

  let s:middle = get(s:, 'middle', &rtp)
  let rtps     = map(s:loaded_names(), 's:rtp(g:bundles[v:val])')
  let afters   = filter(map(copy(rtps), 'globpath(v:val, "after")'), 'isdirectory(v:val)')
  let rtp      = join(map(rtps, 's:escrtp(v:val)'), ',')
                 \ . ','.s:middle.','
                 \ . join(map(afters, 's:escrtp(v:val)'), ',')
  let &rtp     = substitute(substitute(rtp, ',,*', ',', 'g'), '^,\|,$', '', 'g')
  let s:prtp   = &rtp

  if !empty(s:first_rtp)
    execute 'set rtp^='.s:first_rtp
    execute 'set rtp+='.s:last_rtp
  endif
endfunction

function! vundle#load(...)
  if a:0 == 0
    return s:err('Argument missing: plugin name(s) required')
  endif
  if !exists('g:bundles')
    return s:err('vundle#rc was not called')
  endif
  let unknowns = filter(copy(a:000), '!has_key(g:bundles, v:val)')
  if !empty(unknowns)
    let s = len(unknowns) > 1 ? 's' : ''
    return s:err(printf('Unknown plugin%s: %s', s, join(unknowns, ', ')))
  end
  for name in a:000
    call s:lod([name], ['ftdetect', 'after/ftdetect', 'plugin', 'after/plugin'])
  endfor
  doautocmd BufRead
  return 1
endfunction

function! vundle#loadafter(name, delay)
  if get(s:loaded, a:name, 0) | return 0 | endif
  if !exists('g:vundle_delay') | let g:vundle_delay=1 | endif
  if g:vundle_delay >= a:delay
    call s:lod([a:name], ['ftdetect', 'after/ftdetect', 'plugin', 'after/plugin'])
  else
    let g:vundle_delay += 1
  endif
endfunction

function! s:remove_triggers(name)
  if !has_key(s:triggers, a:name)
    return
  endif
  for cmd in s:triggers[a:name].cmd
    execute 'delc' cmd
  endfor
  for map in s:triggers[a:name].map
    execute 'unmap' map
    execute 'iunmap' map
  endfor
  call remove(s:triggers, a:name)
endfunction

function! s:lod(names, types)
  for name in a:names
    call s:remove_triggers(name)
    let s:loaded[name] = 1
  endfor
  call s:reorg_rtp()

  for name in a:names
    let rtp = s:rtp(g:bundles[name])
    for dir in a:types
      call s:source(rtp, dir.'/**/*.vim')
    endfor
  endfor
endfunction

function! s:lod_ft(pat, names)
  call s:lod(a:names, ['plugin', 'after/plugin'])
  execute 'autocmd! VundleLOD FileType' a:pat
  doautocmd filetypeplugin FileType
  doautocmd filetypeindent FileType
endfunction

function! s:lod_cmd(cmd, bang, l1, l2, args, name)
  call s:lod([a:name], ['ftdetect', 'after/ftdetect', 'plugin', 'after/plugin'])
  execute printf('%s%s%s %s', (a:l1 == a:l2 ? '' : (a:l1.','.a:l2)), a:cmd, a:bang, a:args)
endfunction

function! s:lod_map(map, name, prefix)
  call s:lod([a:name], ['ftdetect', 'after/ftdetect', 'plugin', 'after/plugin'])
  let extra = ''
  while 1
    let c = getchar(0)
    if c == 0
      break
    endif
    let extra .= nr2char(c)
  endwhile
  call feedkeys(a:prefix . substitute(a:map, '^<Plug>', "\<Plug>", '') . extra)
endfunction

function! s:add(repo, ...)
  if a:0 > 1
    return s:err('Invalid number of arguments (1..2)')
  endif

  try
    let repo = s:trim(a:repo)
    let name = fnamemodify(repo, ':t:s?\.git$??')
    let spec = extend(s:infer_properties(name, repo),
                    \ a:0 == 1 ? s:parse_options(a:1) : s:base_spec)
    let g:bundles[name] = spec
    let g:bundles_order += [name]
    let s:loaded[name] = 0
  catch
    return s:err(v:exception)
  endtry
endfunction

function! s:parse_options(arg)
  let opts = copy(s:base_spec)
  let type = type(a:arg)
  if type == s:TYPE.string
    let opts.branch = a:arg
  elseif type == s:TYPE.dict
    call extend(opts, a:arg)
    if has_key(opts, 'tag')
      let opts.branch = remove(opts, 'tag')
    endif
    if has_key(opts, 'dir')
      let opts.dir = s:dirpath(expand(opts.dir))
    endif
  else
    throw 'Invalid argument type (expected: string or dictionary)'
  endif
  return opts
endfunction

function! s:infer_properties(name, repo)
  let repo = a:repo
  if s:is_local_bundle(repo)
    return { 'dir': s:dirpath(expand(repo)) }
  else
    if repo =~ ':'
      let uri = repo
    else
      if repo !~ '/'
        let repo = 'vim-scripts/'. repo
      endif
      let fmt = get(g:, 'bundle_url_format', 'https://git::@github.com/%s.git')
      let uri = printf(fmt, repo)
    endif
    let dir = s:dirpath( fnamemodify(join([g:vundle_home, a:name], '/'), ':p') )
    return { 'dir': dir, 'uri': uri }
  endif
endfunction

function! s:install(force, names)
  call vundle#rc()
  call s:update_impl(0, a:force, a:names)
endfunction

function! s:update(force, names)
  call s:update_impl(1, a:force, a:names)
endfunction

function! vundle#helptags()
  if !exists('g:bundles')
    return s:err('vundle#rc was not called')
  endif
  for spec in values(g:bundles)
    let docd = join([spec.dir, 'doc'], '/')
    if isdirectory(docd)
      silent! execute 'helptags' s:esc(docd)
    endif
  endfor
  return 1
endfunction

function! s:syntax()
  syntax clear
  syntax region vundle1 start=/\%1l/ end=/\%2l/ contains=vundleNumber
  syntax region vundle2 start=/\%2l/ end=/\%3l/ contains=vundleBracket,vundleX
  syn match vundleNumber /[0-9]\+[0-9.]*/ contained
  syn match vundleBracket /[[\]]/ contained
  syn match vundleX /x/ contained
  syn match vundleDash /^-/
  syn match vundlePlus /^+/
  syn match vundleStar /^*/
  syn match vundleMessage /\(^- \)\@<=.*/
  syn match vundleName /\(^- \)\@<=[^ ]*:/
  syn match vundleInstall /\(^+ \)\@<=[^:]*/
  syn match vundleUpdate /\(^* \)\@<=[^:]*/
  syn match vundleCommit /^  [0-9a-z]\{7} .*/ contains=vundleRelDate,vundleSha
  syn match vundleSha /\(^  \)\@<=[0-9a-z]\{7}/ contained
  syn match vundleRelDate /([^)]*)$/ contained
  syn match vundleNotLoaded /(not loaded)$/
  syn match vundleError /^x.*/
  syn keyword Function BundleInstall BundleStatus BundleUpdate BundleClean
  hi def link vundle1       Title
  hi def link vundle2       Repeat
  hi def link vundleX       Exception
  hi def link vundleBracket Structure
  hi def link vundleNumber  Number

  hi def link vundleDash    Special
  hi def link vundlePlus    Constant
  hi def link vundleStar    Boolean

  hi def link vundleMessage Function
  hi def link vundleName    Label
  hi def link vundleInstall Function
  hi def link vundleUpdate  Type

  hi def link vundleError   Error
  hi def link vundleRelDate Comment
  hi def link vundleSha     Identifier

  hi def link vundleNotLoaded Comment
endfunction

function! s:lpad(str, len)
  return a:str . repeat(' ', a:len - len(a:str))
endfunction

function! s:lines(msg)
  return split(a:msg, "[\r\n]")
endfunction

function! s:lastline(msg)
  return get(s:lines(a:msg), -1, '')
endfunction

function! s:new_window()
  execute get(g:, 'vundle_window', 'vertical topleft new')
endfunction

function! s:vundle_window_exists()
  let buflist = tabpagebuflist(s:vundle_tab)
  return !empty(buflist) && index(buflist, s:vundle_buf) >= 0
endfunction

function! s:switch_in()
  if !s:vundle_window_exists()
    return 0
  endif

  if winbufnr(0) != s:vundle_buf
    let s:pos = [tabpagenr(), winnr(), winsaveview()]
    execute 'normal!' s:vundle_tab.'gt'
    let winnr = bufwinnr(s:vundle_buf)
    execute winnr.'wincmd w'
    call add(s:pos, winsaveview())
  else
    let s:pos = [winsaveview()]
  endif

  setlocal modifiable
  return 1
endfunction

function! s:switch_out(...)
  call winrestview(s:pos[-1])
  setlocal nomodifiable
  if a:0 > 0
    execute a:1
  endif

  if len(s:pos) > 1
    execute 'normal!' s:pos[0].'gt'
    execute s:pos[1] 'wincmd w'
    call winrestview(s:pos[2])
  endif
endfunction

function! s:prepare()
  call s:job_abort()
  if s:switch_in()
    silent %d _
  else
    call s:new_window()
    nnoremap <silent> <buffer> q  :if b:vundle_preview==1<bar>pc<bar>endif<bar>echo<bar>q<cr>
    nnoremap <silent> <buffer> R  :silent! call <SID>retry()<cr>
    nnoremap <silent> <buffer> D  :BundleDiff<cr>
    nnoremap <silent> <buffer> S  :BundleStatus<cr>
    nnoremap <silent> <buffer> U  :call <SID>status_update()<cr>
    xnoremap <silent> <buffer> U  :call <SID>status_update()<cr>
    nnoremap <silent> <buffer> ]] :silent! call <SID>section('')<cr>
    nnoremap <silent> <buffer> [[ :silent! call <SID>section('b')<cr>
    let b:vundle_preview = -1
    let s:vundle_tab = tabpagenr()
    let s:vundle_buf = winbufnr(0)
    call s:assign_name()
  endif
  silent! unmap <buffer> <cr>
  silent! unmap <buffer> L
  silent! unmap <buffer> X
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap cursorline modifiable
  setf vundle
  call s:syntax()
endfunction

function! s:assign_name()
  " Assign buffer name
  let prefix = '[Bundles]'
  let name   = prefix
  let idx    = 2
  while bufexists(name)
    let name = printf('%s (%s)', prefix, idx)
    let idx = idx + 1
  endwhile
  silent! execute 'f' fnameescape(name)
endfunction

function! s:do(pull, force, todo)
  for [name, spec] in items(a:todo)
    if !isdirectory(spec.dir)
      continue
    endif
    let installed = has_key(s:update.new, name)
    let updated = installed ? 0 :
      \ (a:pull && !empty(s:system_chomp('git log --pretty=format:"%h" "HEAD...HEAD@{1}"', spec.dir)))
    if a:force || installed || updated
      execute 'cd' s:esc(spec.dir)
      call append(3, '- Post-update hook for '. name .' ... ')
      let type = type(spec.do)
      if type == s:TYPE.string
        try
          " FIXME: Escaping is incomplete. We could use shellescape with eval,
          "        but it won't work on Windows.
          let g:_vundle_do = '!'.escape(spec.do, '#!%')
          execute "normal! :execute g:_vundle_do\<cr>\<cr>"
        finally
          let result = v:shell_error ? ('Exit status: '.v:shell_error) : 'Done!'
          unlet g:_vundle_do
        endtry
      elseif type == s:TYPE.funcref
        try
          let status = installed ? 'installed' : (updated ? 'updated' : 'unchanged')
          call spec.do({ 'name': name, 'status': status, 'force': a:force })
          let result = 'Done!'
        catch
          let result = 'Error: ' . v:exception
        endtry
      else
        let result = 'Error: Invalid type!'
      endif
      call setline(4, getline(4) . result)
      cd -
    endif
  endfor
endfunction

function! s:finish(pull)
  let new_frozen = len(filter(keys(s:update.new), 'g:bundles[v:val].frozen'))
  if new_frozen
    let s = new_frozen > 1 ? 's' : ''
    call append(3, printf('- Installed %d frozen bundle%s', new_frozen, s))
  endif
  call append(3, '- Finishing ...')
  redraw
  call vundle#helptags()
  call vundle#end()
  call setline(4, getline(4) . 'Done!')
  redraw
  let msgs = []
  if !empty(s:update.errors)
    call add(msgs, "Press 'R' to retry.")
  endif
  if a:pull && len(s:update.new) < len(filter(getline(5, '$'),
                \ "v:val =~ '^- ' && stridx(v:val, 'Already up-to-date') < 0"))
    call add(msgs, "Press 'D' to see the updated changes.")
  endif
  echo join(msgs, ' ')
endfunction

function! s:retry()
  if empty(s:update.errors)
    return
  endif
  call s:update_impl(s:update.pull, s:update.force,
        \ extend(copy(s:update.errors), [s:update.threads]))
endfunction

function! s:is_managed(name)
  return has_key(g:bundles[a:name], 'uri')
endfunction

function! s:names(...)
  return sort(filter(keys(g:bundles), 'stridx(v:val, a:1) == 0 && s:is_managed(v:val)'))
endfunction

function! s:update_impl(pull, force, args) abort
  let args = copy(a:args)
  let threads = (len(args) > 0 && args[-1] =~ '^[1-9][0-9]*$') ?
                  \ remove(args, -1) : get(g:, 'vundle_threads', 16)

  let managed = filter(copy(g:bundles), 's:is_managed(v:key)')
  let todo = empty(args) ? filter(managed, '!v:val.frozen || !isdirectory(v:val.dir)') :
                         \ filter(managed, 'index(args, v:key) >= 0')

  if empty(todo)
    echohl WarningMsg
    echo 'No bundle to '. (a:pull ? 'update' : 'install') . '.'
    echohl None
    return
  endif

  if !isdirectory(g:vundle_home)
    try
      call mkdir(g:vundle_home, 'p')
    catch
      return s:err(printf('Invalid bundle directory: %s. '.
              \ 'Try to call vundle#rc with a valid directory', g:vundle_home))
    endtry
  endif

  if has('nvim') && !exists('*jobwait') && threads > 1
    echohl WarningMsg
    echomsg 'vundle: update Neovim for parallel installer'
    echohl None
  endif

  let python = (has('python') || has('python3')) && (!s:nvim || has('vim_starting'))
  let ruby   = has('ruby') && !s:nvim && (v:version >= 703 || v:version == 702 && has('patch374')) && !(s:is_win && has('gui_running'))

  let s:update = {
    \ 'start':   reltime(),
    \ 'all':     todo,
    \ 'todo':    copy(todo),
    \ 'errors':  [],
    \ 'pull':    a:pull,
    \ 'force':   a:force,
    \ 'new':     {},
    \ 'threads': (python || ruby || s:nvim) ? min([len(todo), threads]) : 1,
    \ 'bar':     '',
    \ 'fin':     0
  \ }

  call s:prepare()
  call append(0, ['', ''])
  normal! 2G
  silent! redraw

  " Python version requirement (>= 2.7)
  if python && !has('python3') && !ruby && !s:nvim && s:update.threads > 1
    redir => pyv
    silent python import platform; print(platform.python_version())
    redir END
    let python = s:version_requirement(
          \ map(split(split(pyv)[0], '\.'), 'str2nr(v:val)'), [2, 6])
  endif

  if (python || ruby) && s:update.threads > 1 && exists('s:vundle_interactive')
    try
      let imd = &imd
      if s:mac_gui
        set noimd
      endif

      if ruby
          call s:update_ruby()
      else
          "TODO 26-12-2015 13:20 >> fix install method so it looks as nice as
          "the ruby version and make python default (which is ~10% faster)
          call s:update_python()
      endif
    catch
      let lines = getline(4, '$')
      let printed = {}
      silent 4,$d _
      for line in lines
        let name = s:extract_name(line, '.', '')
        if empty(name) || !has_key(printed, name)
          call append('$', line)
          if !empty(name)
            let printed[name] = 1
            if line[0] == 'x' && index(s:update.errors, name) < 0
              call add(s:update.errors, name)
            end
          endif
        endif
      endfor
    finally
      let &imd = imd
      call s:update_finish()
    endtry
  else
    call s:update_vim()
  endif
endfunction

function! s:update_finish()
  if s:switch_in()
    call s:do(s:update.pull, s:update.force, filter(copy(s:update.all), 'has_key(v:val, "do")'))
    call s:finish(s:update.pull)
    call setline(1, 'Updated. Elapsed time: ' . split(reltimestr(reltime(s:update.start)))[0] . ' sec.')
    call s:switch_out('normal! gg')
  endif
endfunction

function! s:job_abort()
  if !s:nvim || !exists('s:jobs')
    return
  endif
  augroup VundleJobControl
    autocmd!
  augroup END
  for [name, j] in items(s:jobs)
    silent! call jobstop(j.jobid)
    if j.new
      call system('rm -rf ' . s:shellesc(g:bundles[name].dir))
    endif
  endfor
  let s:jobs = {}
endfunction

function! s:job_handler(name) abort
  if !s:vundle_window_exists() " bundle window closed
    return s:job_abort()
  endif

  if !has_key(s:jobs, a:name)
    return
  endif
  let job = s:jobs[a:name]

  if v:job_data[1] == 'exit'
    let job.running = 0
    if s:lastline(job.result) ==# 'Error'
      let job.error = 1
      let job.result = substitute(job.result, "Error[\r\n]$", '', '')
    endif
    call s:reap(a:name)
    call s:tick()
  else
    let job.result .= s:to_s(v:job_data[2])
    " To reduce the number of buffer updates
    let job.tick = get(job, 'tick', -1) + 1
    if job.tick % len(s:jobs) == 0
      call s:log(job.new ? '+' : '*', a:name, job.result)
    endif
  endif
endfunction

function! s:spawn(name, cmd, opts)
  let job = { 'running': 1, 'new': get(a:opts, 'new', 0),
            \ 'error': 0, 'result': '' }
  let s:jobs[a:name] = job

  if s:nvim
    let x = jobstart(a:name, 'sh', ['-c',
            \ (has_key(a:opts, 'dir') ? s:with_cd(a:cmd, a:opts.dir) : a:cmd)
            \ . ' || echo Error'])
    if x > 0
      let job.jobid = x
      augroup VundleJobControl
        execute 'autocmd JobActivity' a:name printf('call s:job_handler(%s)', string(a:name))
      augroup END
    else
      let job.running = 0
      let job.error   = 1
      let job.result  = x < 0 ? 'sh is not executable' :
            \ 'Invalid arguments (or job table is full)'
    endif
  else
    let params = has_key(a:opts, 'dir') ? [a:cmd, a:opts.dir] : [a:cmd]
    let job.result = call('s:system', params)
    let job.error = v:shell_error != 0
    let job.running = 0
  endif
endfunction

function! s:reap(name)
  if s:nvim
    silent! execute 'autocmd! VundleJobControl JobActivity' a:name
  endif

  let job = s:jobs[a:name]
  if job.error
    call add(s:update.errors, a:name)
  elseif get(job, 'new', 0)
    let s:update.new[a:name] = 1
  endif
  let s:update.bar .= job.error ? 'x' : '='

  call s:log(job.error ? 'x' : '-', a:name, job.result)
  call s:bar()

  call remove(s:jobs, a:name)
endfunction

function! s:bar()
  if s:switch_in()
    let total = len(s:update.all)
    call setline(1, (s:update.pull ? 'Updating' : 'Installing').
          \ ' bundles ('.len(s:update.bar).'/'.total.')')
    call s:progress_bar(2, s:update.bar, total)
    call s:switch_out()
  endif
endfunction

function! s:logpos(name)
  for i in range(1, line('$'))
    if getline(i) =~# '^[-+x*] '.a:name.':'
      return i
    endif
  endfor
  return 0
endfunction

function! s:log(bullet, name, lines)
  if s:switch_in()
    let pos = s:logpos(a:name)
    if pos > 0
      execute pos 'd _'
      if pos > winheight('.')
        let pos = 4
      endif
    else
      let pos = 4
    endif
    call append(pos - 1, s:format_message(a:bullet, a:name, a:lines))
    call s:switch_out()
  endif
endfunction

function! s:update_vim()
  let s:jobs = {}

  call s:bar()
  call s:tick()
endfunction

function! s:tick()
while 1 " Without TCO, Vim stack is bound to explode
  if empty(s:update.todo)
    if empty(s:jobs) && !s:update.fin
      let s:update.fin = 1
      call s:update_finish()
    endif
    return
  endif

  let name = keys(s:update.todo)[0]
  let spec = remove(s:update.todo, name)
  let pull = s:update.pull
  let new  = !isdirectory(spec.dir)

  if exists('s:vundle_interactive') | call s:log(new ? '+' : '*', name, pull ? 'Updating ...' : 'Installing ...') | endif
  redraw

  if !new
    let [valid, msg] = s:git_valid(spec, 0)
    if valid
      if pull
        call s:spawn(name,
          \ printf('git checkout -q %s 2>&1 && git pull --progress --no-rebase origin %s 2>&1 && git submodule update --init --recursive 2>&1',
          \ s:shellesc(spec.branch), s:shellesc(spec.branch)), { 'dir': spec.dir })
      else
        let s:jobs[name] = { 'running': 0, 'result': 'Already installed', 'error': 0 }
      endif
    else
      let s:jobs[name] = { 'running': 0, 'result': msg, 'error': 1 }
    endif
  else
    call s:spawn(name,
          \ printf('git clone --depth=1 --progress --recursive %s -b %s %s 2>&1',
          \ s:shellesc(spec.uri),
          \ s:shellesc(spec.branch),
          \ s:shellesc(s:trim(spec.dir))), { 'new': 1 })
  endif

  if !s:jobs[name].running
    call s:reap(name)
  endif
  if len(s:jobs) >= s:update.threads
    break
  endif
endwhile
endfunction

function! s:update_ruby()
  ruby << EOF
  module VundleStream
    SEP = ["\r", "\n", nil]
    def get_line
      buffer = ''
      loop do
        char = readchar rescue return
        if SEP.include? char.chr
          buffer << $/
          break
        else
          buffer << char
        end
      end
      buffer
    end
  end unless defined?(VundleStream)

  def esc arg
    %["#{arg.gsub('"', '\"')}"]
  end

  def killall pid
    pids = [pid]
    if /mswin|mingw|bccwin/ =~ RUBY_PLATFORM
      pids.each { |pid| Process.kill 'INT', pid.to_i rescue nil }
    else
      unless `command -v pgrep 2> /dev/null`.empty?
        children = pids
        until children.empty?
          children = children.map { |pid|
            `pgrep -P #{pid}`.lines.map { |l| l.chomp }
          }.flatten
          pids += children
        end
      end
      pids.each { |pid| Process.kill 'TERM', pid.to_i rescue nil }
    end
  end

  require 'thread'
  require 'fileutils'
  require 'timeout'
  running = true
  iswin = VIM::evaluate('s:is_win').to_i == 1
  pull  = VIM::evaluate('s:update.pull').to_i == 1
  base  = VIM::evaluate('g:vundle_home')
  all   = VIM::evaluate('s:update.todo')
  limit = VIM::evaluate('get(g:, "vundle_timeout", 60)')
  tries = VIM::evaluate('get(g:, "vundle_retries", 2)') + 1
  nthr  = VIM::evaluate('s:update.threads').to_i
  maxy  = VIM::evaluate('winheight(".")').to_i
  cd    = iswin ? 'cd /d' : 'cd'
  tot   = VIM::evaluate('len(s:update.todo)') || 0
  bar   = ''
  skip  = 'Already installed'
  mtx   = Mutex.new
  take1 = proc { mtx.synchronize { running && all.shift } }
  logh  = proc {
    cnt = bar.length
    $curbuf[1] = "#{pull ? 'Updating' : 'Installing'} bundles (#{cnt}/#{tot})"
    $curbuf[2] = '[' + bar.ljust(tot) + ']'
    VIM::command('normal! 2G')
    VIM::command('redraw') unless iswin
  }
  where = proc { |name| (1..($curbuf.length)).find { |l| $curbuf[l] =~ /^[-+x*] #{name}:/ } }
  log   = proc { |name, result, type|
    mtx.synchronize do
      ing  = ![true, false].include?(type)
      bar += type ? '=' : 'x' unless ing
      b = case type
          when :install  then '+' when :update then '*'
          when true, nil then '-' else
            VIM::command("call add(s:update.errors, '#{name}')")
            'x'
          end
      result =
        if type || type.nil?
          ["#{b} #{name}: #{result.lines.to_a.last}"]
        elsif result =~ /^Interrupted|^Timeout/
          ["#{b} #{name}: #{result}"]
        else
          ["#{b} #{name}"] + result.lines.map { |l| "    " << l }
        end
      if lnum = where.call(name)
        $curbuf.delete lnum
        lnum = 4 if ing && lnum > maxy
      end
      result.each_with_index do |line, offset|
        $curbuf.append((lnum || 4) - 1 + offset, line.gsub(/\e\[./, '').chomp)
      end
      logh.call
    end
  }
  bt = proc { |cmd, name, type, cleanup|
    tried = timeout = 0
    begin
      tried += 1
      timeout += limit
      fd = nil
      data = ''
      if iswin
        Timeout::timeout(timeout) do
          tmp = VIM::evaluate('tempname()')
          system("#{cmd} > #{tmp}")
          data = File.read(tmp).chomp
          File.unlink tmp rescue nil
        end
      else
        fd = IO.popen(cmd).extend(VundleStream)
        first_line = true
        log_prob = 1.0 / nthr
        while line = Timeout::timeout(timeout) { fd.get_line }
          data << line
          log.call name, line.chomp, type if name && (first_line || rand < log_prob)
          first_line = false
        end
        fd.close
      end
      [$? == 0, data.chomp]
    rescue Timeout::Error, Interrupt => e
      if fd && !fd.closed?
        killall fd.pid
        fd.close
      end
      cleanup.call if cleanup
      if e.is_a?(Timeout::Error) && tried < tries
        3.downto(1) do |countdown|
          s = countdown > 1 ? 's' : ''
          log.call name, "Timeout. Will retry in #{countdown} second#{s} ...", type
          sleep 1
        end
        log.call name, 'Retrying ...', type
        retry
      end
      [false, e.is_a?(Interrupt) ? "Interrupted!" : "Timeout!"]
    end
  }
  main = Thread.current
  threads = []
  watcher = Thread.new {
    while VIM::evaluate('getchar(1)')
      sleep 0.1
    end
    mtx.synchronize do
      running = false
      threads.each { |t| t.raise Interrupt }
    end
    threads.each { |t| t.join rescue nil }
    main.kill
  }
  refresh = Thread.new {
    while true
      mtx.synchronize do
        break unless running
        VIM::command('noautocmd normal! a')
      end
      sleep 0.2
    end
  } if VIM::evaluate('s:mac_gui') == 1

  progress = iswin ? '' : '--progress'
  nthr.times do
    mtx.synchronize do
      threads << Thread.new {
        while pair = take1.call
          name = pair.first
          dir, uri, branch, tag = pair.last.values_at *%w[dir uri branch tag]
          checkout = esc(tag ? tag : branch)
          merge = esc(tag ? tag : "origin/#{branch}")
          subm = "git submodule update --init --recursive 2>&1"
          exists = File.directory? dir
          ok, result =
            if exists
              chdir = "#{cd} #{iswin ? dir : esc(dir)}"
              ret, data = bt.call "#{chdir} && git rev-parse --abbrev-ref HEAD 2>&1 && git config remote.origin.url", nil, nil, nil
              current_uri = data.lines.to_a.last
              if !ret
                if data =~ /^Interrupted|^Timeout/
                  [false, data]
                else
                  [false, [data.chomp, "BundleClean required."].join($/)]
                end
              elsif current_uri.sub(/git::?@/, '') != uri.sub(/git::?@/, '')
                [false, ["Invalid URI: #{current_uri}",
                         "Expected:    #{uri}",
                         "BundleClean required."].join($/)]
              else
                if pull
                  log.call name, 'Updating ...', :update
                  fetch_opt = (tag && File.exist?(File.join(dir, '.git/shallow'))) ? '--depth 99999999' : ''
                  bt.call "#{chdir} && git fetch #{fetch_opt} --progress 2>&1 && git checkout -q #{checkout} 2>&1 && git merge --ff-only #{merge} 2>&1 && #{subm}", name, :update, nil
                else
                  [true, skip]
                end
              end
            else
              d = esc dir.sub(%r{[\\/]+$}, '')
              log.call name, 'Installing ...', :install
              bt.call "git clone --depth=1 #{progress} --recursive #{uri} -b #{branch} #{d} 2>&1", name, :install, proc {
                FileUtils.rm_rf dir
              }
            end
          mtx.synchronize { VIM::command("let s:update.new['#{name}'] = 1") } if !exists && ok
          log.call name, result, ok
        end
      } if running
    end
  end
  threads.each { |t| t.join rescue nil }
  logh.call
  refresh.kill if refresh
  watcher.kill
EOF
endfunction

function! s:update_python()
let py_exe = has('python') ? 'python' : 'python3'
execute py_exe "<< EOF"
import datetime
import functools
import os
try:
  import queue
except ImportError:
  import Queue as queue
import random
import re
import shutil
import signal
import subprocess
import tempfile
import threading as thr
import time
import traceback
import vim

G_NVIM = vim.eval("has('nvim')") == '1'
G_PULL = vim.eval('s:update.pull') == '1'
G_RETRIES = int(vim.eval('get(g:, "vundle_retries", 2)')) + 1
G_TIMEOUT = int(vim.eval('get(g:, "vundle_timeout", 60)'))
G_LOG_PROB = 1.0 / int(vim.eval('s:update.threads'))
G_STOP = thr.Event()
G_IS_WIN = vim.eval('s:is_win') == '1'

class VundleError(Exception):
  def __init__(self, msg):
    self.msg = msg
class CmdTimedOut(VundleError):
  pass
class CmdFailed(VundleError):
  pass
class InvalidURI(VundleError):
  pass
class Action(object):
  INSTALL, UPDATE, ERROR, DONE = ['+', '*', 'x', '-']

class Buffer(object):
  def __init__(self, lock, num_bundles, is_pull):
    self.bar = ''
    self.event = 'Updating' if is_pull else 'Installing'
    self.lock = lock
    self.maxy = int(vim.eval('winheight(".")'))
    self.num_bundles = num_bundles

  def __where(self, name):
    """ Find first line with name in current buffer. Return line num. """
    found, lnum = False, 0
    matcher = re.compile('^[-+x*] {0}:'.format(name))
    for line in vim.current.buffer:
      if matcher.search(line) is not None:
        found = True
        break
      lnum += 1

    if not found:
      lnum = -1
    return lnum

  def header(self):
    curbuf = vim.current.buffer
    curbuf[0] = self.event + ' plugins ({0}/{1})'.format(len(self.bar), self.num_bundles)

    num_spaces = self.num_bundles - len(self.bar)
    curbuf[1] = '[{0}{1}]'.format(self.bar, num_spaces * ' ')

    with self.lock:
      vim.command('normal! 2G')
      vim.command('redraw')

  def write(self, action, name, lines):
    first, rest = lines[0], lines[1:]
    msg = ['{0} {1}{2}{3}'.format(action, name, ': ' if first else '', first)]
    msg.extend(['    ' + line for line in rest])

    try:
      if action == Action.ERROR:
        self.bar += 'x'
        vim.command("call add(s:update.errors, '{0}')".format(name))
      elif action == Action.DONE:
        self.bar += '='

      curbuf = vim.current.buffer
      lnum = self.__where(name)
      if lnum != -1: # Found matching line num
        del curbuf[lnum]
        if lnum > self.maxy and action in set([Action.INSTALL, Action.UPDATE]):
          lnum = 3
      else:
        lnum = 3
      curbuf.append(msg, lnum)

      self.header()
    except vim.error:
      pass

class Command(object):
  CD = 'cd /d' if G_IS_WIN else 'cd'

  def __init__(self, cmd, cmd_dir=None, timeout=60, cb=None, clean=None):
    self.cmd = cmd
    if cmd_dir:
      self.cmd = '{0} {1} && {2}'.format(Command.CD, cmd_dir, self.cmd)
    self.timeout = timeout
    self.callback = cb if cb else (lambda msg: None)
    self.clean = clean if clean else (lambda: None)
    self.proc = None

  @property
  def alive(self):
    """ Returns true only if command still running. """
    return self.proc and self.proc.poll() is None

  def execute(self, ntries=3):
    """ Execute the command with ntries if CmdTimedOut.
        Returns the output of the command if no Exception.
    """
    attempt, finished, limit = 0, False, self.timeout

    while not finished:
      try:
        attempt += 1
        result = self.try_command()
        finished = True
        return result
      except CmdTimedOut:
        if attempt != ntries:
          self.notify_retry()
          self.timeout += limit
        else:
          raise

  def notify_retry(self):
    """ Retry required for command, notify user. """
    for count in range(3, 0, -1):
      if G_STOP.is_set():
        raise KeyboardInterrupt
      msg = 'Timeout. Will retry in {0} second{1} ...'.format(
            count, 's' if count != 1 else '')
      self.callback([msg])
      time.sleep(1)
    self.callback(['Retrying ...'])

  def try_command(self):
    """ Execute a cmd & poll for callback. Returns list of output.
        Raises CmdFailed   -> return code for Popen isn't 0
        Raises CmdTimedOut -> command exceeded timeout without new output
    """
    first_line = True

    try:
      tfile = tempfile.NamedTemporaryFile(mode='w+b')
      preexec_fn = not G_IS_WIN and os.setsid or None
      self.proc = subprocess.Popen(self.cmd, stdout=tfile,
                                   stderr=subprocess.STDOUT,
                                   stdin=subprocess.PIPE, shell=True,
                                   preexec_fn=preexec_fn)
      thrd = thr.Thread(target=(lambda proc: proc.wait()), args=(self.proc,))
      thrd.start()

      thread_not_started = True
      while thread_not_started:
        try:
          thrd.join(0.1)
          thread_not_started = False
        except RuntimeError:
          pass

      while self.alive:
        if G_STOP.is_set():
          raise KeyboardInterrupt

        if first_line or random.random() < G_LOG_PROB:
          first_line = False
          line = '' if G_IS_WIN else nonblock_read(tfile.name)
          if line:
            self.callback([line])

        time_diff = time.time() - os.path.getmtime(tfile.name)
        if time_diff > self.timeout:
          raise CmdTimedOut(['Timeout!'])

        thrd.join(0.5)

      tfile.seek(0)
      result = [line.decode('utf-8', 'replace').rstrip() for line in tfile]

      if self.proc.returncode != 0:
        raise CmdFailed([''] + result)

      return result
    except:
      self.terminate()
      raise

  def terminate(self):
    """ Terminate process and cleanup. """
    if self.alive:
      if G_IS_WIN:
        os.kill(self.proc.pid, signal.SIGINT)
      else:
        os.killpg(self.proc.pid, signal.SIGTERM)
    self.clean()

class Bundle(object):
  def __init__(self, name, args, buf_q, lock):
    self.name = name
    self.args = args
    self.buf_q = buf_q
    self.lock = lock
    tag = args.get('tag', 0)
    self.checkout = esc(tag if tag else args['branch'])
    self.merge = esc(tag if tag else 'origin/' + args['branch'])
    self.tag = tag

  def manage(self):
    try:
      if os.path.exists(self.args['dir']):
        self.update()
      else:
        self.install()
        with self.lock:
          thread_vim_command("let s:update.new['{0}'] = 1".format(self.name))
    except VundleError as exc:
      self.write(Action.ERROR, self.name, exc.msg)
    except KeyboardInterrupt:
      G_STOP.set()
      self.write(Action.ERROR, self.name, ['Interrupted!'])
    except:
      # Any exception except those above print stack trace
      msg = 'Trace:\n{0}'.format(traceback.format_exc().rstrip())
      self.write(Action.ERROR, self.name, msg.split('\n'))
      raise

  def install(self):
    target = self.args['dir']
    if target[-1] == '\\':
      target = target[0:-1]

    def clean(target):
      def _clean():
        try:
          shutil.rmtree(target)
        except OSError:
          pass
      return _clean

    self.write(Action.INSTALL, self.name, ['Installing ...'])
    callback = functools.partial(self.write, Action.INSTALL, self.name)
    cmd = 'git clone --depth=1 --progress --recursive {0} -b {1} {2} 2>&1'.format(
          '' if self.tag else self.args['uri'], self.checkout, esc(target))
    com = Command(cmd, None, G_TIMEOUT, callback, clean(target))
    result = com.execute(G_RETRIES)
    self.write(Action.DONE, self.name, result[-1:])

  def repo_uri(self):
    cmd = 'git rev-parse --abbrev-ref HEAD 2>&1 && git config remote.origin.url'
    command = Command(cmd, self.args['dir'], G_TIMEOUT,)
    result = command.execute(G_RETRIES)
    return result[-1]

  def update(self):
    match = re.compile(r'git::?@')
    actual_uri = re.sub(match, '', self.repo_uri())
    expect_uri = re.sub(match, '', self.args['uri'])
    if actual_uri != expect_uri:
      msg = ['',
             'Invalid URI: {0}'.format(actual_uri),
             'Expected     {0}'.format(expect_uri),
             'BundleClean required.']
      raise InvalidURI(msg)

    if G_PULL:
      self.write(Action.UPDATE, self.name, ['Updating ...'])
      callback = functools.partial(self.write, Action.UPDATE, self.name)
      fetch_opt = '--depth 99999999' if self.tag and os.path.isfile(os.path.join(self.args['dir'], '.git/shallow')) else ''
      cmds = ['git fetch {0} --progress'.format(fetch_opt),
              'git checkout -q {0}'.format(self.checkout),
              'git merge --ff-only {0}'.format(self.merge),
              'git submodule update --init --recursive']
      cmd = ' 2>&1 && '.join(cmds)
      com = Command(cmd, self.args['dir'], G_TIMEOUT, callback)
      result = com.execute(G_RETRIES)
      self.write(Action.DONE, self.name, result[-1:])
    else:
      self.write(Action.DONE, self.name, ['Already installed'])

  def write(self, action, name, msg):
    self.buf_q.put((action, name, msg))

class VundleThread(thr.Thread):
  def __init__(self, tname, args):
    super(VundleThread, self).__init__()
    self.tname = tname
    self.args = args

  def run(self):
    thr.current_thread().name = self.tname
    buf_q, work_q, lock = self.args

    try:
      while not G_STOP.is_set():
        name, args = work_q.get_nowait()
        bundle = Bundle(name, args, buf_q, lock)
        bundle.manage()
        work_q.task_done()
    except queue.Empty:
      pass

class RefreshThread(thr.Thread):
  def __init__(self, lock):
    super(RefreshThread, self).__init__()
    self.lock = lock
    self.running = True

  def run(self):
    while self.running:
      with self.lock:
        thread_vim_command('noautocmd normal! a')
      time.sleep(0.33)

  def stop(self):
    self.running = False

if G_NVIM:
  def thread_vim_command(cmd):
    vim.session.threadsafe_call(lambda: vim.command(cmd))
else:
  def thread_vim_command(cmd):
    vim.command(cmd)

def esc(name):
  return '"' + name.replace('"', '\"') + '"'

def nonblock_read(fname):
  """ Read a file with nonblock flag. Return the last line. """
  fread = os.open(fname, os.O_RDONLY | os.O_NONBLOCK)
  buf = os.read(fread, 100000).decode('utf-8', 'replace')
  os.close(fread)

  line = buf.rstrip('\r\n')
  left = max(line.rfind('\r'), line.rfind('\n'))
  if left != -1:
    left += 1
    line = line[left:]

  return line

def main():
  thr.current_thread().name = 'main'
  nthreads = int(vim.eval('s:update.threads'))
  bundles = vim.eval('s:update.todo')
  mac_gui = vim.eval('s:mac_gui') == '1'

  lock = thr.Lock()
  buf = Buffer(lock, len(bundles), G_PULL)
  buf_q, work_q = queue.Queue(), queue.Queue()
  for work in bundles.items():
    work_q.put(work)

  start_cnt = thr.active_count()
  for num in range(nthreads):
    tname = 'VundleT-{0:02}'.format(num)
    thread = VundleThread(tname, (buf_q, work_q, lock))
    thread.start()
  if mac_gui:
    rthread = RefreshThread(lock)
    rthread.start()

  while not buf_q.empty() or thr.active_count() != start_cnt:
    try:
      action, name, msg = buf_q.get(True, 0.25)
      buf.write(action, name, msg)
      buf_q.task_done()
    except queue.Empty:
      pass
    except KeyboardInterrupt:
      G_STOP.set()

  if mac_gui:
    rthread.stop()
    rthread.join()

main()
EOF
endfunction

function! s:shellesc(arg)
  return '"'.substitute(a:arg, '"', '\\"', 'g').'"'
endfunction

function! s:glob_dir(path)
  return map(filter(s:lines(globpath(a:path, '**')), 'isdirectory(v:val)'), 's:dirpath(v:val)')
endfunction

function! s:progress_bar(line, bar, total)
  call setline(a:line, '[' . s:lpad(a:bar, a:total) . ']')
endfunction

function! s:compare_git_uri(a, b)
  let a = substitute(a:a, 'git:\{1,2}@', '', '')
  let b = substitute(a:b, 'git:\{1,2}@', '', '')
  return a ==# b
endfunction

function! s:format_message(bullet, name, message)
  if a:bullet != 'x'
    return [printf('%s %s: %s', a:bullet, a:name, s:lastline(a:message))]
  else
    let lines = map(s:lines(a:message), '"    ".v:val')
    return extend([printf('x %s:', a:name)], lines)
  endif
endfunction

function! s:with_cd(cmd, dir)
  return 'cd '.s:esc(a:dir).' && '.a:cmd
endfunction

function! s:system(cmd, ...)
  let cmd = a:0 > 0 ? s:with_cd(a:cmd, a:1) : a:cmd
  return system(s:is_win ? '('.cmd.')' : cmd)
endfunction

function! s:system_chomp(...)
  let ret = call('s:system', a:000)
  return v:shell_error ? '' : substitute(ret, '\n$', '', '')
endfunction

function! s:git_valid(spec, check_branch)
  let ret = 1
  let msg = 'OK'
  if isdirectory(a:spec.dir)
    let result = s:lines(s:system('git rev-parse --abbrev-ref HEAD 2>&1 && git config remote.origin.url', a:spec.dir))
    let remote = result[-1]
    if v:shell_error
      let msg = join([remote, 'BundleClean required.'], "\n")
      let ret = 0
    elseif !s:compare_git_uri(remote, a:spec.uri)
      let msg = join(['Invalid URI: '.remote,
                    \ 'Expected:    '.a:spec.uri,
                    \ 'BundleClean required.'], "\n")
      let ret = 0
    elseif a:check_branch
      let branch = result[0]
      if a:spec.branch !=# branch
        let tag = s:system_chomp('git describe --exact-match --tags HEAD 2>&1', a:spec.dir)
        if a:spec.branch !=# tag
          let msg = printf('Invalid branch/tag: %s (expected: %s). Try BundleUpdate.',
                \ (empty(tag) ? branch : tag), a:spec.branch)
          let ret = 0
        endif
      endif
    endif
  else
    let msg = 'Not found'
    let ret = 0
  endif
  return [ret, msg]
endfunction

function! s:clean(force)
  call s:prepare()
  call append(0, 'Searching for unused bundles in '.g:vundle_home)
  call append(1, '')

  " List of valid directories
  let dirs = []
  let [cnt, total] = [0, len(g:bundles)]
  for [name, spec] in items(g:bundles)
    if !s:is_managed(name) || s:git_valid(spec, 0)[0]
      call add(dirs, spec.dir)
    endif
    let cnt += 1
    call s:progress_bar(2, repeat('=', cnt), total)
    normal! 2G
    redraw
  endfor

  let allowed = {}
  for dir in dirs
    let allowed[dir] = 1
    for child in s:glob_dir(dir)
      let allowed[child] = 1
    endfor
  endfor

  let todo = []
  let found = sort(s:glob_dir(g:vundle_home))
  while !empty(found)
    let f = remove(found, 0)
    if !has_key(allowed, f) && isdirectory(f)
      call add(todo, f)
      call append(line('$'), '- ' . f)
      let found = filter(found, 'stridx(v:val, f) != 0')
    end
  endwhile

  normal! G
  redraw
  if empty(todo)
    call append(line('$'), 'Already clean.')
  else
    call inputsave()
    let yes = a:force || (input('Proceed? (y/N) ') =~? '^y')
    call inputrestore()
    if yes
      for dir in todo
        if isdirectory(dir)
          call system((s:is_win ? 'rmdir /S /Q ' : 'rm -rf ') . s:shellesc(dir))
        endif
      endfor
      call append(line('$'), 'Removed.')
    else
      call append(line('$'), 'Cancelled.')
    endif
  endif
  normal! G
endfunction

function! s:upgrade_specs()
  for spec in values(g:bundles)
    let spec.frozen = get(spec, 'frozen', 0)
  endfor
endfunction

function! s:status()
  call s:prepare()
  call append(0, 'Checking bundles')
  call append(1, '')

  let ecnt = 0
  let unloaded = 0
  let [cnt, total] = [0, len(g:bundles)]
  for [name, spec] in items(g:bundles)
    if has_key(spec, 'uri')
      if isdirectory(spec.dir)
        let [valid, msg] = s:git_valid(spec, 1)
      else
        let [valid, msg] = [0, 'Not found. Try BundleInstall.']
      endif
    else
      if isdirectory(spec.dir)
        let [valid, msg] = [1, 'OK']
      else
        let [valid, msg] = [0, 'Not found.']
      endif
    endif
    let cnt += 1
    let ecnt += !valid
    " `s:loaded` entry can be missing if BundleUpgraded
    if valid && get(s:loaded, name, -1) == 0
      let unloaded = 1
      let msg .= ' (not loaded)'
    endif
    call s:progress_bar(2, repeat('=', cnt), total)
    call append(3, s:format_message(valid ? '-' : 'x', name, msg))
    normal! 2G
    redraw
  endfor
  call setline(1, 'Finished. '.ecnt.' error(s).')
  normal! gg
  setlocal nomodifiable
  if unloaded
    echo "Press 'L' on each line to load bundle, or 'U' to update"
    nnoremap <silent> <buffer> L :call <SID>status_load(line('.'))<cr>
    xnoremap <silent> <buffer> L :call <SID>status_load(line('.'))<cr>
  end
endfunction

function! s:extract_name(str, prefix, suffix)
  return matchstr(a:str, '^'.a:prefix.' \zs[^:]\+\ze:.*'.a:suffix.'$')
endfunction

function! s:status_load(lnum)
  let line = getline(a:lnum)
  let name = s:extract_name(line, '-', '(not loaded)')
  if !empty(name)
    call vundle#load(name)
    setlocal modifiable
    call setline(a:lnum, substitute(line, ' (not loaded)$', '', ''))
    setlocal nomodifiable
  endif
endfunction

function! s:status_update() range
  let lines = getline(a:firstline, a:lastline)
  let names = filter(map(lines, 's:extract_name(v:val, "[x-]", "")'), '!empty(v:val)')
  if !empty(names)
    echo
    execute 'BundleUpdate' join(names)
  endif
endfunction

function! s:is_preview_window_open()
  silent! wincmd P
  if &previewwindow
    wincmd p
    return 1
  endif
  return 0
endfunction

function! s:find_name(lnum)
  for lnum in reverse(range(1, a:lnum))
    let line = getline(lnum)
    if empty(line)
      return ''
    endif
    let name = s:extract_name(line, '-', '')
    if !empty(name)
      return name
    endif
  endfor
  return ''
endfunction

function! s:preview_commit()
  if b:vundle_preview < 0
    let b:vundle_preview = !s:is_preview_window_open()
  endif

  let sha = matchstr(getline('.'), '\(^  \)\@<=[0-9a-z]\{7}')
  if empty(sha)
    return
  endif

  let name = s:find_name(line('.'))
  if empty(name) || !has_key(g:bundles, name) || !isdirectory(g:bundles[name].dir)
    return
  endif

  execute 'pedit' sha
  wincmd P
  setlocal filetype=git buftype=nofile nobuflisted
  execute 'silent read !cd' s:esc(g:bundles[name].dir) '&& git show' sha
  normal! gg"_dd
  wincmd p
endfunction

function! s:section(flags)
  call search('\(^[x-] \)\@<=[^:]\+:', a:flags)
endfunction

function! s:diff()
  call s:prepare()
  call append(0, 'Collecting updated changes ...')
  normal! gg
  redraw

  let cnt = 0
  for [k, v] in items(g:bundles)
    if !isdirectory(v.dir) || !s:is_managed(k)
      continue
    endif

    let diff = s:system_chomp('git log --pretty=format:"%h %s (%cr)" "HEAD...HEAD@{1}"', v.dir)
    if !empty(diff)
      call append(1, '')
      call append(2, '- '.k.':')
      call append(3, map(s:lines(diff), '"  ". v:val'))
      let cnt += 1
      normal! gg
      redraw
    endif
  endfor

  call setline(1, cnt == 0 ? 'No updates.' : 'Last update:')
  nnoremap <silent> <buffer> <cr> :silent! call <SID>preview_commit()<cr>
  nnoremap <silent> <buffer> X    :call <SID>revert()<cr>
  normal! gg
  setlocal nomodifiable
  if cnt > 0
    echo "Press 'X' on each block to revert the update"
  endif
endfunction

function! s:revert()
  let name = s:find_name(line('.'))
  if empty(name) || !has_key(g:bundles, name) ||
    \ input(printf('Revert the update of %s? (y/N) ', name)) !~? '^y'
    return
  endif

  call s:system('git reset --hard HEAD@{1} && git checkout '.s:esc(g:bundles[name].branch), g:bundles[name].dir)
  setlocal modifiable
  normal! "_dap
  setlocal nomodifiable
  echo 'Reverted.'
endfunction

function! s:snapshot(...) abort
  let home = get(s:, 'vundle_home_org', g:vundle_home)
  let [type, var, header] = s:is_win ?
    \ ['dosbatch', '%vundle_home%',
    \   ['@echo off', ':: Generated by vundle', ':: '.strftime("%c"), '',
    \    ':: Make sure to BundleUpdate first', '', 'set vundle_home='.s:esc(home)]] :
    \ ['sh', '$vundle_home',
    \   ['#!/bin/bash',  '# Generated by vundle', '# '.strftime("%c"), '',
    \    'vim +BundleUpdate +qa', '', 'vundle_home='.s:esc(home)]]

  call s:prepare()
  execute 'setf' type
  call append(0, header)
  call append('$', '')
  1
  redraw

  let dirs = sort(map(values(filter(copy(g:bundles),
        \'has_key(v:val, "uri") && isdirectory(v:val.dir)')), 'v:val.dir'))
  let anchor = line('$') - 1
  for dir in reverse(dirs)
    let sha = s:system_chomp('git rev-parse --short HEAD', dir)
    if !empty(sha)
      call append(anchor, printf('cd %s && git reset --hard %s',
            \ substitute(dir, '^'.g:vundle_home, var, ''), sha))
      redraw
    endif
  endfor

  if a:0 > 0
    let fn = s:esc(expand(a:1))
    call writefile(getline(1, '$'), fn)
    if !s:is_win | call system('chmod +x ' . fn) | endif
    echo 'Saved to '.a:1
    silent execute 'e' fn
  endif
endfunction

function! s:split_rtp()
  return split(&rtp, '\\\@<!,')
endfunction

let s:first_rtp = s:escrtp(get(s:split_rtp(), 0, ''))
let s:last_rtp  = s:escrtp(get(s:split_rtp(), -1, ''))

if exists('g:bundles')
  let g:bundles_order = get(g:, 'bundles_order', keys(g:bundles))
  call s:upgrade_specs()
  call s:define_commands()
endif

let &cpo = s:cpo_save
unlet s:cpo_save

