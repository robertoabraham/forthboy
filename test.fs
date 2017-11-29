\ SDL test page

: array ( n -- ) ( name: n -- a-addr ) create cells allot DOES> swap cells + ;

160 CONSTANT WIDTH
144 CONSTANT HEIGHT

WIDTH HEIGHT * array pixels

: sdl-test ( -- )
  SDL_INIT_VIDEO SDL_Init ( error? )
  IF ." SDL_Init failure: " SDL_GetError c-str> type CR THEN
  s" hello world" >c-str   ( c-addr )
  100 100 WIDTH HEIGHT SDL_WINDOW_SHOWN SDL_CreateWindow ( win )

  dup -1 SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC OR SDL_CreateRenderer ( win ren )

  dup SDL_PIXELFORMAT_ARGB8888 SDL_TEXTUREACCESS_STREAMING WIDTH HEIGHT SDL_CreateTexture ( win ren tex )

  \ Populate the pixels array.
  \ Default color: solid blue.
  $FF0000FF
  WIDTH HEIGHT * 0 DO dup i pixels ! LOOP
  drop

  dup   0   0 pixels   WIDTH cells  SDL_UpdateTexture ( win ren tex error? )
  IF ." UpdateTexture failed: " SDL_GetError c-str> type cr THEN

  over SDL_RenderClear drop
  2dup 0 0 SDL_RenderCopy drop
  over SDL_RenderPresent

  4000 SDL_Delay

  ( win ren tex )
  SDL_DestroyTexture
  SDL_DestroyRenderer
  SDL_DestroyWindow
  SDL_Quit
;

sdl-test bye
