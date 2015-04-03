\ Forth GB (c) 2015 Braden Shepherdson
\ Version 1
\ Gameboy graphics unit emulation

\ The only API I expose is gpu-draw-line ( -- ) which draws a single line
\ on the display. That should only be called while LY is a visible row, and
\ the display is enabled.

\ DRAWING
\ BG color 0 is always at the back. Sprite color 0 is always transparent.
\ The window overlays the background, but they're adjacent in the layering.
\ Sprite colors 1-3 might be in front of or behind the background, depending
\ on the priority bit in its attributes.

\ Therefore we need to make a pass over the sprites, collecting them into
\ two lists: under-background and above-background.
\ There is a maximum total of 10, with the leftmost sprites the most important.
\ Ties (ie. same X) are broken by table ordering - closer to the beginning
\ of the table wins.
\ Also sprites with offscreen Y coordinates are hidden. Offscreen X-coordinates
\ still count for priority and the maximum of 10!

\ 8x16 mode pairs up sprites, essentially, putting 2k+1 just underneath 2k.

\ The plan:
\ - Run through the sprites in REVERSE address order. For each one visible on
\   this line, insert the sprite into a table by X-coordinate.
\ - Scan this table, starting at the leftmost X-coordinate and counting the
\   number of sprites seen. After 10, stop scanning. Those sprites are the ones
\   visible in this line.
\ - Loop over the pixels RIGHT TO LEFT, computing each time the current sprite
\   and then determining the stacking order based on that sprite's priority
\   relative to the BG.
\ - The colour of a pixel is the first of:
\   - The sprite, when above the background and the colour is nonzero.
\   - The window, even is zero, when sprites are below.
\   - The background, when nonzero and sprites are below.
\   - The sprite, if nonzero and below the background.
\   - BG 0, when the sprite is transparent, and the window is not covering.

\ Once we've determined what the color number is, we convert it to a real color
\ using either the BG palette (BG and window) or one of the two sprite palettes,
\ which is determined by a sprite attribute bit.

160 CONSTANT WIDTH
144 CONSTANT HEIGHT

VARIABLE sdl-window
VARIABLE sdl-renderer
VARIABLE sdl-texture

\ The number of sprites on the current line.
VARIABLE sprite-count
\ Ordered list of sprites, leftmost first.
10 CARRAY active-sprites
\ Table of sprites by X-coordinate. Should be populated in reverse address order
\ so that sprites which have the same coordinate are written lowest-address-last
\ which matches the priority ordering.
WIDTH CARRAY sprites-by-x

\ Array of real 32-bit ARGB pixels.
\ To be flipped into the SDL texture and then onto the screen.
WIDTH HEIGHT * ARRAY pixels

\ Defining a sprite structure, since it's used all over the place.
: sprite ( number -- sprite ) 2 LSHIFT oam ;
: sprite-y ( sprite -- u ) C@ ;
: sprite-x ( sprite -- u ) 1+ C@ ;
: sprite-tile ( sprite -- u ) 2 + C@ ;
: sprite-attrs  ( sprite -- u ) 3 + C@ ;

128 CONSTANT sprite-mask-priority
 64 CONSTANT sprite-mask-y-flip
 32 CONSTANT sprite-mask-x-flip
 16 CONSTANT sprite-palette-number

: blank-sprites-by-x
  0 sprites-by-x DUP WIDTH + SWAP DO 255 I C! LOOP \ Sprite 0 is valid. 255 is not
;

: blank-everything
  0 active-sprites DUP 10 + SWAP DO 0 I C! LOOP
  0 sprite-count !
  blank-sprites-by-x
;

\ Returns a Y-range suitable for WITHIN. Any sprite whose Y-coordinate lies
\ WITHIN these is visible.
\ Remember that the stored Y position is Y-16.
: y-range ( -- top bottom )
  io-LCDC io-ports C@ lcdc-OBJ-SIZE
  IF 15 ELSE 7 THEN   ( height )
  io-LY io-ports C@   ( height ly )
  16 -                ( height real-ly )
  2DUP SWAP -         ( height ly top )
  -ROT NIP            ( top ly )
  1+                  ( top bottom )
;

: scan-all-sprites
  y-range    ( top bottom )
  39 -1 DO
    I sprite DUP sprite-y ( top bottom sprite Y )
    2OVER WITHIN          ( top bottom sprite ? )
    IF
      sprite-x               ( top bottom X )
      I SWAP sprites-by-x C! ( top bottom )
    ELSE DROP THEN
  -1 +LOOP
;

: count-up-sprites
  \ Now run through sprites-by-x from left to right.
  160 0 DO
    I sprites-by-x C@ ( number )
    DUP 255 <> IF
      sprite                ( sprite )
      sprite-count @        ( sprite index )
      2DUP active-sprites ! ( sprite index )
      1+ DUP sprite-count ! ( sprite index' )
      10 = IF 2DROP UNLOOP EXIT THEN \ Bail if we reach 10.
      DROP ( )
    ELSE DROP THEN
  LOOP
;

\ Runs through the sprite list, right to left. For each sprite,
\ writes out its index into sprites-by-x for all pixels it covers.
: iterate-sprites
  blank-sprites-by-x
  \ Now iterate the active-sprites list backwards.
  -1   sprite-count @ 1- DO
    I active-sprites @ ( sprite )
    DUP sprite-x       ( sprite x )
    \ Runs from x to x+7
    dup 7 + swap DO
      dup i sprites-by-x c! ( sprite )
    LOOP
    drop ( )
  -1 +LOOP
;




\ Main pixel-drawing routines.


\ Flow chart:
\ 1. No sprite, draw BG.
\ 2. Sprite
\   a. Sprite priority 1 - find sprite colour
\     i.  Sprite colour is nonzero, draw it and done.
\     ii. Sprite colour is zero, draw BG.
\   b. Sprite priority 0 - find BG color
\     i.  BG is nonzero, draw it.
\     ii. BG is zero, draw sprite.
\       A. Sprite color is zero, draw BG.
\       B. Sprite color is nonzero, draw it.
\ The simple approach is to find the priority and both colors, and do
\ all the logic in one place.
\ If I can find the colors cheaply, that would be ideal.

: bg-color ( y x -- n-color )
;

: 

: palette-bg ( n-color -- real-color )
;
: palette-sprite ( sprite n-color -- real-color )
;

: paint-pixel ( y x real-color -- )
  rot WIDTH * rot + ( real-color index )
  pixels ! ( )
;

\ Paints the background color, including reading its color.
: draw-bg ( y x -- )
  2dup bg-color palette-bg ( y x real-color )
  paint-pixel ( )
;

: draw-pixel ( y x -- )
  dup sprites-by-x c@ ( y x n-sprite )
  dup 255 = IF
    drop draw-bg
  ELSE
    sprite dup sprite-attrs


    >r 2dup 2dup bg-at ( y x y x bg-color   R: n-sprite )
    r> swap >r    ( y x y x n-sprite   R: bg-color )
    sprite        ( y x y x sprite     R: bg-color )
    dup >r        ( y x y x sprite     R: bg-color sprite )
    sprite-at     ( y x sprite-color   R: bg-color sprite )
    r> r>         ( y x sprite-color sprite bg-color )
    \ Now check the sprite priority.
    over sprite-attrs sprite-mask-priority ( y x s-color s bg-color priority )

    \ START HERE: lots of juggling. Maybe a better way to do this as a
    \ flow-chart with multiple words in some chain? Think about this when more
    \ awake.
  drop bg-at palette-bg
;


: draw-line ( )
  \ Work along the current line, drawing pixels.
  io-LY io-ports C@ ( y )
  WIDTH 0 DO dup i draw-pixel LOOP
  drop
;

: gpu-draw-line ( -- )
  blank-everything \ Reset all the state.
  scan-all-sprites \ Populates sprites-by-x
  count-up-sprites \ Populates sprites and sets sprite-count
  iterate-sprites  \ Rewrites sprites-by-x where each entry is the sprite number
                   \ of the sprite covering that spot.
  draw-line        \ Draws pixel by pixel.
;


\ Called at the very start of VBlank, before any user CPU code runs.
\ Flips the drawn frame onto the GPU.
: gpu-draw-frame ( -- )
  sdl-texture @   0   0 pixels   WIDTH cells ( tex rect array pitch )
  SDL_UpdateTexture ( error? )
  IF ." SDL_UpdateTexture error: " SDL_GetError c-str> type cr THEN ( )

  sdl-renderer @ ( ren )
  dup SDL_RenderClear drop ( ren )
  dup sdl-texture @ 0 0 SDL_RenderCopy ( ren )
  SDL_RenderPresent ( )
;




\ Does the necessary SDL legwork. Sets up the pixel array, the renderer,
\ texture, and so on.
: gpu-init ( -- )
  SDL_INIT_VIDEO SDL_Init
  IF ." SDL_Init failure: " SDL_GetError c-str> type cr THEN

  s" hello world" >c-str
  100 100 WIDTH HEIGHT SDL_WINDOW_SHOWN SDL_CreateWindow ( win )
  dup sdl-window !

  -1 SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC OR
  SDL_CreateRenderer ( ren )
  dup sdl-renderer !

  SDL_PIXELFORMAT_ARGB8888 SDL_TEXTUREACCESS_STREAMING WIDTH HEIGHT
  SDL_CreateTexture ( tex )
  sdl-texture !
;

: gpu-cleanup ( -- )
  sdl-texture  @ SDL_DestroyTexture
  sdl-renderer @ SDL_DestroyRenderer
  sdl-window   @ SDL_DestroyWindow
  SDL_Quit
;

