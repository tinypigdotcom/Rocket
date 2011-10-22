#!/cygdrive/c/Perl/bin/perl.exe

###################################################################
#                                                                 #
# rocket.pl - Land a rocket ship on a moving platform             #
# Copyright (c) 2003 David Bradford.                              #
#                                                                 #
# David Bradford - Tinypig software - www.tinypig.com             #
#                                                                 #
# All rights reserved.  This program is free software; you can    #
# redistribute it and/or modify it under the same terms as Perl   #
# itself; however, you must leave this copyright statement        #
# intact.                                                         #
#                                                                 #
###################################################################
# TODO
# * perlcritic
# * remove awful "sub" implementation of constants like HEIGHT()
# * create package as was done for tkquiz.pl

## no critic (ProhibitMultiplePackages, RequireFilenameMatchesPackage)
# Reason:  I want this game contained in a single script.
#          These classes are not suitable for re-use.

use strict;
use warnings;
use Carp;

#####################
# Class: Game_Piece #
#####################

# This is the base class for game_pieces
package Game_Piece;

# Class:  Game_Piece
# Method: r
# Return a random integer from 1 to the first parameter.
sub r {
    my ( $this, $num ) = @_;
    return int( rand($num) + 1 );
}

###############
# Class: Base #
###############
package Base;
#use vars qw(@ISA);
#@ISA = ('Game_Piece');
use base 'Game_Piece';

# Constants

# Class:  Base
# Method: new
# Called to instantiate the class.
sub new {
    my ( $class, $screen, $width ) = @_;
    my $SIDE_WIDTH = 10;
    ## no critic (ProhibitCommaSeparatedStatements)
    # Reason: bug in Perl::Critic itself.
    my $this       = bless {
        'inc'        => 0,
        'move'       => 1,
        'screen'     => $screen,
        'width'      => $width,
        'HEIGHT'     => 10,
        'SIDE_WIDTH' => $SIDE_WIDTH,
        'BOTH_SIDES' => $SIDE_WIDTH * 2,
    }, $class;
    ## use critic

    # initialize

    return $this;
}

# Class:  Base
# Method: calcMove
# This routine calculates where the base should move next
sub calcMove {
    my ( $this, $hpos, $move ) = @_;

    if ( $$hpos + $$move < 1 ) { $$move = $this->{inc} }
    elsif ( $$hpos + $$move >
        $main::SCR_WIDTH - $this->{width} - $this->{BOTH_SIDES} )
    {
        $$move = -$this->{inc};
    }
    $$hpos += $$move;

    return;
}

# Class:  Base
# Method: getHpos
# Returns the horizontal position of the base in pixels.
sub getHpos {
    my $this = shift;
    return ( $this->{hpos} + $this->{SIDE_WIDTH} );
}

# Class:  Base
# Method: getMove
# Returns the move attribute.
sub getMove {
    my $this = shift;
    return $this->{move};
}

# Class:  Base
# Method: getWidth
# Set the width attribute of the base
sub getWidth {
    my $this = shift;
    return $this->{width};
}

# Class:  Base
# Method: initPosition
# Prepares the base to be drawn in a random horizontal position.
sub initPosition {
    my $this = shift;
    $this->{hpos} =
      $this->r( $main::SCR_WIDTH - $this->{width} - $this->{BOTH_SIDES} );

    return;
}

# Class:  Base
# Method: make
# Draw the base.
sub make {
    my ( $this, $color ) = @_;
    $this->{screen}->erase('base');
    $this->{screen}->createPolygon(
        $this->{hpos} + 0,                                    500,
        $this->{hpos} + $this->{SIDE_WIDTH},                  490,
        $this->{hpos} + $this->{SIDE_WIDTH} + $this->{width}, 490,
        $this->{hpos} + $this->{BOTH_SIDES} + $this->{width}, 500,
        -fill    => $color,
        -outline => 'white',
        -tags    => [ 'base', 'base_and_rocket', 'everything' ]
    );
    $this->{screen}->lower( 'base', 'everything' );

    return;
}

# Class:  Base
# Method: move
# This routine will reverse the direction of the base if it has hit either side of
# the screen, and also moves the base one "tick" in whatever direction it is going.
sub move {
    my ( $this, $tag ) = @_;
    if ( !$main::DEBUG ) {
        $this->calcMove( \$this->{hpos}, \$this->{move} );
        $this->{screen}->move( $tag, $this->{move}, 0 );
    }

    return;
}

# Class: Base
# Method: predictPosition
# Predicts where the base will be n ticks from now
sub predictPosition {
    my ( $this, $n ) = @_;
    my $hpos = $this->{hpos};
    my $move = $this->{move};
    for ( my $i = 1 ; $i < $n ; ++$i ) {
        $this->calcMove( \$hpos, \$move );
    }
    return $hpos;
}

# Class:  Base
# Method: setIncrement
# Sets the number of pixels the base will move each "tick".  This is used
# to increase the difficulty throughout the game.
sub setIncrement {
    my ( $this, $inc ) = @_;
    $this->{inc} = $inc;
    $this->{move} = $this->{move} < 0 ? -$inc : $inc;

    return;
}

# Class:  Base
# Method: setWidth
# Set the width attribute of the base
sub setWidth {
    my ( $this, $width ) = @_;
    $this->{width} = $width;

    return;
}

#################
# Class: Rocket #
#################

package Rocket;
#use vars qw(@ISA);
#@ISA = ('Game_Piece');
use base 'Game_Piece';

# Constants

# Class:  Rocket
# Method: new
# Called to instantiate the class.
sub new {
    my ( $class, $screen ) = @_;
    ## no critic (ProhibitCommaSeparatedStatements)
    # Reason: bug in Perl::Critic itself.
    my $this = bless {
        'screen'       => $screen,
        'everyother'   => 0,
        'screen_width' => $main::SCR_WIDTH,
        'HEIGHT'       => 55,
        'WIDTH'        => 40,
        'BASECOLOR'    => 'blue',
        'CONECOLOR'    => 'purple',
        'FINCOLOR'     => 'dark grey',
    }, $class;
    ## use critic

    $this->initPosition();
    return $this;
}

# Class:  Rocket
# Method: changeDirection
# The rocket is always be in motion.  When a player decides to
# change direction or just go straight down, this routine is
# called.
sub changeDirection {
    my ( $this, $dir ) = @_;
    if ( $dir eq 'left' ) {
        $this->{move} = -$main::INCREMENT;
    }
    elsif ( $dir eq 'right' ) {
        $this->{move} = $main::INCREMENT;
    }
    elsif ( $dir eq 'down' ) {
        $this->{move} = 0;
    }

    return;
}

# Class:  Rocket
# Method: crashTick
# When the rocket crashes, each time this routine is called it
# moves each of the pieces one "tick" in their course of being
# blown apart.
sub crashTick {
    my ($this) = @_;
    $this->_movePiece( 'p1', -1, -1 );
    $this->_movePiece( 'p2', 1,  -1 );
    $this->_movePiece( 'p3', -1, $this->{everyother} % 2 * -1 );
    $this->_movePiece( 'p4', 1,  $this->{everyother} % 2 * -1 );
    $this->_movePiece( 'p5', -1, $this->{everyother} % 2 * 1 );
    $this->_movePiece( 'p6', 1,  $this->{everyother} % 2 * 1 );
    $this->_movePiece( 'p7', -1, 1 );
    $this->_movePiece( 'p8', 1,  1 );
    $this->{everyother} = $this->{everyother} ? 0 : 1;

    return;
}

# Class:  Rocket
# Method: erase
# Deletes the rocket from the screen after it explodes or
# lands successfully
sub erase {
    my $this = shift;
    $this->{screen}->clearRocket();

    return;
}

# Class:  Rocket
# Method: getHpos
# Get the horizontal position of the rocket in pixels.
sub getHpos {
    my $this = shift;
    return $this->{hpos};
}

# Class:  Rocket
# Method: getVpos
# Get the vertical position of the rocket in pixels.
sub getVpos {
    my $this = shift;
    return $this->{vpos};
}

# Class:  Rocket
# Method: initPosition
# Prepares the rocket to be drawn in a random horizontal initial
# position at the top of the screen, preparing it to begin descent.
sub initPosition {
    my $this = shift;
    $this->{move} = 0;
    $this->{vpos} = 0;
    if ($main::DEBUG) {
        $this->{vpos} = ($main::SCR_HEIGHT) - $this->{HEIGHT} - 10 - 3;
    }
    $this->{hpos} = $this->r( ($main::SCR_WIDTH) - $this->{WIDTH} );

    return;
}

# Class:  Rocket
# Method: makeRocket
# Draw the rocket for the first time.  These polygons will be manipulated after
# creation to animate them and only need to be "drawn" initially once, or after
# the rocket is erased.
sub makeRocket {
    my ($this) = @_;
    $this->_makePiece( 'p1', $this->{CONECOLOR}, 10, 15, 20, 0,  20, 15 );
    $this->_makePiece( 'p2', $this->{CONECOLOR}, 20, 0,  30, 15, 20, 15 );
    $this->_makePiece( 'p3', $this->{BASECOLOR}, 10, 15, 10, 30, 20, 30, 20,
        15 );
    $this->_makePiece( 'p4', $this->{BASECOLOR}, 20, 15, 30, 15, 30, 30, 20,
        30 );
    $this->_makePiece( 'p5', $this->{BASECOLOR}, 10, 30, 20, 30, 20, 45, 10,
        45 );
    $this->_makePiece( 'p6', $this->{BASECOLOR}, 20, 30, 30, 30, 30, 45, 20,
        45 );
    $this->_makePiece( 'p7', $this->{FINCOLOR}, 10, 45, 0, 55, 20, 55, 20, 45 );
    $this->_makePiece( 'p8', $this->{FINCOLOR}, 30, 45, 40, 55, 20, 55, 20,
        45 );
    $this->{screen}->raise( 'rocket', 'base' );

    return;
}

# Class:  Rocket
# Method: move
# Moves the rocket for one "tick".  Also makes sure it doesn't move beyond
# the edge of the screen horizontally.
sub move {
    my ($this) = @_;
    my $inc = $main::INCREMENT;
    if ($main::DEBUG) { $inc = 0; }
    if (   $this->{hpos} + $this->{move} > 0
        && $this->{hpos} + $this->{move} <=
        $this->{screen_width} - $this->{WIDTH} + $main::RIGHT_SIDE_FIX )
    {

        $this->{screen}->move( 'rocket', $this->{move}, $inc );
        $this->{hpos} += $this->{move};
    }
    else {
        $this->{screen}->move( 'rocket', 0, $inc );
    }
    $this->{vpos} += $inc;

    return;
}

# Class:  Rocket
# Method: _makePiece
# Used internally by instances of this class and called by makeRocket, this
# does the actual work of drawing each polygon in the rocket.
sub _makePiece {
    my ( $this, $tag, $color, @points ) = @_;
    for ( my $i = 0 ; $i < $#points ; $i += 2 ) {
        $points[$i]       += $this->{hpos};
        $points[ $i + 1 ] += $this->{vpos};
    }
    $this->{screen}->createPolygon(
        @points,
        -fill => $color,
        -tags =>
          [ $tag, 'rocket', 'base_and_rocket', 'everything', 'game_pieces' ]
    );

    return;
}

# Class:  Rocket
# Method: _movePiece
# Used internally by instances of this class and called by crashTick, this
# actually does the work of moving the individual pieces one "tick" after
# a crash.
sub _movePiece {
    my ( $this, $piece_tag, $x_multip, $y_multip ) = @_;
    $this->{screen}->move(
        $piece_tag,
        $x_multip * $this->r($main::EXPLODE_MOVE),
        $y_multip * $this->r($main::EXPLODE_MOVE)
    );

    return;
}

#################
# Class: Screen #
#################
package Screen;

# Class:  Screen
# Method: new
# Called to instantiate the class.
sub new {
    my ( $class, $MW ) = @_;
    my $this;
    my $canvas = $MW->Canvas(
        -width      => $main::SCR_WIDTH,
        -height     => $main::SCR_HEIGHT,
        -border     => 1,
        -relief     => 'ridge',
        -background => 'black'
    )->pack();

    my $image = $MW->Photo(-format => 'bmp', -file => "$main::ROCKET_DIR/stars.bmp");

        $canvas->createImage(250,250,-image => $image);

    $this = bless { 'canvas' => $canvas }, $class;
    return $this;
}

# Class:  Screen
# Method: afterCancel
# Cancels a specific alarm that is set, or all alarms.  These alarms are
# set to cause routines like "tick" to be called by the OS every few milliseconds
# in order to animate the game.
sub afterCancel {
    my ($this,@args) = @_;
    $this->{canvas}->afterCancel(@args);

    return;
}

# Class:  Screen
# Method: clearGamePieces
# Clears all game pieces from the screen.
sub clearGamePieces {
    my $this = shift;
    $this->{canvas}->delete('game_pieces');

    return;
}

# Class:  Screen
# Method: clearMessages
# Clears all messages from the screen.
sub clearMessages {
    my $this = shift;
    $this->{canvas}->delete('messages');

    return;
}

# Class:  Screen
# Method: clearRocket
# Clears the rocket from the screen.
sub clearRocket {
    my $this = shift;
    $this->{canvas}->delete('rocket');

    return;
}

# Class:  Screen
# Method: createPolygon
# Allows game piece objects to draw a polygon on the canvas.
sub createPolygon {
    my ($this,@args) = @_;
    $this->{canvas}->createPolygon(@args);

    return;
}

# Class:  Screen
# Method: erase
# Allows game piece objects to erase themselves from the canvas.
sub erase {
    my ($this,@args) = @_;
    $this->{canvas}->delete(@args);

    return;
}

# Method: lower
# Lower an object in relation to other objects
sub lower {
    my ($this,@args) = @_;
    $this->{canvas}->lower(@args);

    return;
}

# Class:  Screen
# Method: move
# Used by the game piece objects to move themselves across the canvas.
sub move {
    my ($this,@args) = @_;
    $this->{canvas}->move(@args);

    return;
}

# Class:  Screen
# Method: raise
# Raise an object in relation to other objects
sub raise {
    my ($this,@args) = @_;
    $this->{canvas}->raise(@args);

    return;
}

# Class:  Screen
# Method: showDebug
# Displayes the debugging info
sub showDebug {
    my ( $this, @arg ) = @_;
    $this->{canvas}->delete('debug');
    $this->_showText( 'debug', 100, 80,  "Rocket vpos: $arg[0]" );
    $this->_showText( 'debug', 100, 100, "Rocket hpos: $arg[1]" );
    $this->_showText( 'debug', 100, 120, "Base hpos: $arg[2]" );
    $this->_showText( 'debug', 100, 140, "Status: $arg[3]" );

    $this->_showText( 'debug', 350, 80,  "Base width: $arg[4]" );
    $this->_showText( 'debug', 350, 100, "Base extra: $arg[5]" );
    my $debug_text = <<"EOF";
if( \$rocket->getHpos() >= ( \$base->getHpos() - \$SUCCESS_PLAY ) &&
\$rocket->getHpos() + \$rocket->WIDTH <=
( \$base->getHpos() + \$base->getWidth() + \$SUCCESS_PLAY ) ) { ok }
EOF
    $this->_showText(
        'debug', 240, 200, $debug_text
    );

    return;
}

# Class:  Screen
# Method: showGameOver
# This displays the Game Over message.
sub showGameOver {
    my $this = shift;
    $this->_showText( 'message', 250, 200, "GAME\nOVER" );

    return;
}

# Class:  Screen
# Method: showGuys
# Draws the icons representing each life (rocket) the player has left.
sub showGuys {
    my ( $this, $rocket, $guys ) = @_;
    $this->{canvas}->delete('guys');
    for ( 1 .. $guys - 1 ) {
        my $guy_hval = $_ * 20 + 320;
        $this->{canvas}->createPolygon(
            $guy_hval + 7,  15,
            $guy_hval + 12, 9,
            $guy_hval + 17, 15,
            -fill => $rocket->{CONECOLOR},
            -tags => [ 'guys', 'everything' ]
        );
        $this->{canvas}->createPolygon(
            $guy_hval + 7,  15,
            $guy_hval + 17, 15,
            $guy_hval + 17, 25,
            $guy_hval + 7,  25,
            -fill => $rocket->{BASECOLOR},
            -tags => [ 'guys', 'everything' ]
        );
        $this->{canvas}->createPolygon(
            $guy_hval + 7,  25,
            $guy_hval + 17, 25,
            $guy_hval + 22, 30,
            $guy_hval + 2,  30,
            $guy_hval + 7,  25,
            -fill => $rocket->{FINCOLOR},
            -tags => [ 'guys', 'everything' ]
        );
    }

    return;
}

# Class:  Screen
# Method: showPause
# Displays the "PAUSE" message.
sub showPause {
    my $this = shift;
    $this->_showText( 'message', 250, 200, 'PAUSE', 'pause' );

    return;
}

# Class:  Screen
# Method: showScores
# Displays the score and the high score.
sub showScores {
    my ( $this, $score, $high ) = @_;
    my ( $display_high, $display_score );
    $this->erase('scores');
    $score ||= 0;
    $display_score = sprintf( "%6d", $score );
    $display_high  = sprintf( "%6d", $high );
    $this->_showText( 'score', 80,  41, $display_score );
    $this->_showText( 'score', 250, 41, $display_high );

    return;
}

# Class:  Screen
# Method: showTitles
# Displayes the "SCORE" and "HIGH" titles.
sub showTitles {
    my $this = shift;
    $this->_showText( 'title', 250, 16, 'HIGH' );
    $this->_showText( 'title', 80,  16, 'SCORE' );

    return;
}

# Class:  Screen
# Method: _showText
# Used internally by instances of this class to display certain
# types of text on the screen.
sub _showText {
    my ( $this, $type, $x, $y, $text, $extra_tag ) = @_;
    if ( $type eq 'message' ) {
        $this->{canvas}->createText(
            $x, $y,
            -fill => 'white',
            -font => 'Arial 20 bold',
            -text => $text,
            -tags => [ 'messages', 'everything', $extra_tag ]
        );
    }
    elsif ( $type eq 'score' ) {
        $this->{canvas}->createText(
            $x, $y,
            -fill => 'white',
            -font => 'Arial 20 bold',
            -text => $text,
            -tags => [ 'scores', 'everything', $extra_tag ]
        );
    }
    elsif ( $type eq 'title' ) {
        $this->{canvas}->createText(
            $x, $y,
            -fill => 'red',
            -font => 'Arial 10 bold',
            -text => $text,
            -tags => [ 'titles', 'everything', $extra_tag ]
        );
    }
    elsif ( $type eq 'debug' ) {
        $this->{canvas}->createText(
            $x, $y,
            -fill => 'red',
            -font => 'Arial 10 bold',
            -text => $text,
            -tags => [ 'debug', 'everything', $extra_tag ]
        );
    }

    return;
}

########
# MAIN #
########

package main;
use Tk;
use Tk::Dialog;

#use Tk::JPEG;

my ( $anim_seq, $autopilot, $base, $cheat, %config );
my ( $guys, $nosound );
my ( $pause_cb, $rocket, $score, $screen, $sound_c, %timer_id, $width );
my ( $frame, $hmenu, $menu, $MW );    # window variables

# Constants
our $BWIDTH     = 10;
our $DEBUG      = 0;
our $PADX       = 5;
our $PADY       = 3;
our $FONT       = 'Arial 8 normal';
our $COPYRIGHT  = '(c) 2003 David Bradford, Tinypig Software (www.tinypig.com)';
our $ROCKET_DIR = '.';
our $VERSION    = '1.03';

# game constants
our $AUTO_DOWN_RAND       = 30;
our $AUTO_WRONG_DEVIATE   = 50;
our $AUTO_WRONG_DONE_RAND = 100;
our $AUTO_WRONG_RAND      = 30;
our $BASE_RAND            = 3;
our $CRASH_DELAY          = 15;
our $CRASH_FRAMES         = 55;
our $EXPLODE_MOVE         = 6;
our $EXTRA_GUY            = 1000;
our $INCREMENT            = 2;
our $MAX_GUYS             = 5;
our $PIECE_SPACING        = 2;
our $RIGHT_SIDE_FIX       = 4;
our $SCR_WIDTH            = 500;
our $SCR_HEIGHT           = 500;
our $SUCCESS_FRAME_MIN    = 200;
our $SUCCESS_FRAME_PLAY   = 100;
our $SUCCESS_PLAY         = 8;
our $SUCCESS_POINTS       = 100;
our $TICK_DELAY           = 15;
our $INI_FILE             = 'rocket.ini';
our $CRASH_SOUND          = 'arcade11.wav';
our $EXTRA_GUY_SOUND      = 'arcade07.wav';
our $LANDING_SOUND        = 'arcade02.wav';

# Initialization
my $auto_down      = 0;
my $gameover       = 0;
my $level          = 0;
my $paused         = 0;
my $purpose        = 0;
my $success_frames = 0;
my @b_colors       = ( 'green', 'magenta', 'blue', 'red', 'turquoise' );
my @b_increments   = ( 3, 4, 5, 6, 7 );
my @next_level     = ( 400, 800, 1200, 1600, 999999 );
my @width          = ( 58, 56, 54, 52, 50 );
my $auto_wrong     = 0;

readConfig();

if ( eval { require Win32::Sound; 1 } ) {
    $nosound = 0;
}
else {
    $nosound = 1;
    $config{sound} = 0;
}
$sound_c = $config{sound};

setupWindow();

$screen = Screen->new($MW);

# Debugging key binds
$MW->bind(
    '<s>' => sub {
        $score += $SUCCESS_POINTS;
        $screen->showScores( $score, $config{high} );
    }
);
$MW->bind(
    '<a>' => sub {
        $score -= $SUCCESS_POINTS;
        $screen->showScores( $score, $config{high} );
    }
);
$MW->bind( '<f>' => sub { ++$guys; $screen->showGuys( $rocket, $guys ) } );
$MW->bind( '<d>' =>
      sub { $guys -= $guys > 1 ? 1 : 0; $screen->showGuys( $rocket, $guys ) } );

# game key binds
$MW->bind( '<x>' => sub { exit } );
$MW->bind( '<n>'     => \&startGame );
$MW->bind( '<p>'     => \&pause );
$MW->bind( '<Left>'  => sub { $rocket->changeDirection('left') } );
$MW->bind( '<Right>' => sub { $rocket->changeDirection('right') } );
$MW->bind( '<Down>'  => sub { $rocket->changeDirection('down') } );
$MW->bind(
    '<Up>' => sub {
        if   ( $cheat = $cheat ? 0 : 1 ) { calcAutopilot() }
        else                             { $autopilot = 0 }
    }
);

# menu key binds
$MW->bind( '<Alt-Key-r>' => sub { $menu->Post;  Tk::Menu->Unpost } );
$MW->bind( '<Alt-Key-h>' => sub { $hmenu->Post; Tk::Menu->Unpost } );

$base = Base->new( $screen, $width[$level] );
$rocket = Rocket->new($screen);

$screen->showTitles();
$base->initPosition();

# run as if game is over until a new game is started
gameOver();

# MainLoop gives control back to windows.
MainLoop;

# Subroutine: tick
# This is the main subroutine in the game.  Each time it is called,
# game pieces move their alloted distance and checks are made
# for success or failure.  This is out of alphabetical order because
# it is the main routine.
sub tick {
    if ($paused) {
        $timer_id{t} = $MW->after( $TICK_DELAY, \&tick );
    }
    else {

        # if the game is over, auto-pilot the rocket
        if ($autopilot) {

            # Here I am trying to make the auto-pilot look a little
            # more human by giving it the wrong target location
            # every so often
            if ( r($AUTO_WRONG_RAND) == 1 && !$auto_wrong && !$cheat ) {
                if ( r(2) == 1 ) {
                    my $scr_max =
                      $SCR_WIDTH - $rocket->{WIDTH} + $RIGHT_SIDE_FIX -
                      $base->{SIDE_WIDTH};
                    my $scr_min = $base->{SIDE_WIDTH};
                    $auto_wrong = $autopilot - $AUTO_WRONG_DEVIATE +
                      r( $AUTO_WRONG_DEVIATE * 2 );
                    if ( $auto_wrong < $scr_min ) { $auto_wrong = $scr_min }
                    if ( $auto_wrong > $scr_max ) { $auto_wrong = $scr_max }
                }
                else {
                    $auto_down = r($AUTO_DOWN_RAND);
                }
            }
            my $goal = $auto_wrong ? $auto_wrong : $autopilot;
            if ( abs( $rocket->getHpos() - $goal ) < 2 || $auto_down ) {
                $rocket->changeDirection('down');
                if ($auto_wrong) {
                    if ( r($AUTO_WRONG_DONE_RAND) == 1 ) { $auto_wrong = 0 }
                }
                if ($auto_down) { --$auto_down }
            }
            elsif ( $rocket->getHpos() < $goal ) {
                $rocket->changeDirection('right');
            }
            elsif ( $rocket->getHpos() > $goal ) {
                $rocket->changeDirection('left');
            }
        }
        $rocket->move();
        if ($DEBUG) {
            $rocket->changeDirection('down');
            my $status = detectCrash() ? 'land' : 'CRASH';
            $screen->showDebug(
                $rocket->getVpos(), $rocket->getHpos(),
                $base->getHpos(),   $status,
                $base->getWidth(),  $base->{BOTH_SIDES},
            );
        }
        if (
            $rocket->getVpos() < (
                $SCR_HEIGHT - $rocket->{HEIGHT} - $base->{HEIGHT} -
                  $PIECE_SPACING
            )
          )
        {
            $base->move('base');
            $timer_id{t} = $MW->after( $TICK_DELAY, \&tick );
        }
        else {
            $auto_wrong = 0;
            if ( detectCrash() ) {
                success();
            }
            else {
                crash();
            }
        }
    }

    return;
}

sub calcAutopilot {

    # calculate number of ticks until landing
    my $k = int(
        (
            (
                $SCR_HEIGHT - $rocket->{HEIGHT} - $base->{HEIGHT} - 2 -
                  $rocket->getVpos()
            ) / $INCREMENT
        ) + .5
    );

    # calculate horizontal position rocket needs to be at
    $autopilot =
      $base->predictPosition($k) + $base->{SIDE_WIDTH} + r($SUCCESS_PLAY);
    if ( !$cheat ) { $auto_down = r($AUTO_DOWN_RAND) }

    return;
}

# Subroutine: crash
# This routine is called when a crash is detected.
sub crash {
    --$guys unless $gameover;
    $anim_seq = 1;
    playSound($CRASH_SOUND);
    $success_frames = r($SUCCESS_FRAME_PLAY) + $SUCCESS_FRAME_MIN;
    crashAnim();

    return;
}

# Subroutine: crashAnim
# Called by crash, this routine is like the "tick" routine for the
# crash animation (the explosion and the base moving back and forth).
sub crashAnim {
    if ( !$paused ) {
        ++$anim_seq;
        if ( $anim_seq <= $CRASH_FRAMES ) {
            $rocket->crashTick();
            $base->move('base');
            $timer_id{c} = $MW->after( $CRASH_DELAY, \&crashAnim );
        }
        else {
            $rocket->erase();
            if ( $anim_seq <= $success_frames ) {
                $base->move('base');
                $timer_id{c} = $MW->after( $CRASH_DELAY, \&crashAnim );
            }
            else {
                if ( $guys > 0 ) {
                    init();
                }
                else {
                    gameOver();
                }
            }
        }
    }
    else {
        $timer_id{c} = $MW->after( $CRASH_DELAY, \&crashAnim );
    }

    return;
}

# Subroutine: detectCrash
# Tell us if we are not aligned correctly with the base,
# and ready for a crash.
sub detectCrash {
    if (   $rocket->getHpos() >= ( $base->getHpos() - $SUCCESS_PLAY )
        && $rocket->getHpos() + $rocket->{WIDTH} <=
        ( $base->getHpos() + $base->getWidth() + $SUCCESS_PLAY ) )
    {
        return 1;
    }
    return 0;
}

# Subroutine: gameOver
# This routine is called after a crash, when it is determined you have
# no more "guys" left.
sub gameOver {
    $screen->showGameOver();
    startGame(1);

    return;
}

# Subroutine: gameOverAnim
# Called by gameOver, this routine is like the "tick" routine for the
# game over animation (the base just keeps moving back and forth
# until a new game is started).
sub gameOverAnim {
    $base->move('base');
    $timer_id{c} = $MW->after( $CRASH_DELAY, \&gameOverAnim );

    return;
}

# Subroutine: init
# This routine resets the board after a successful landing, a crash,
# or the beginning of the game.
sub init {
    $rocket->initPosition();
    for ( keys %timer_id ) { $screen->afterCancel( $timer_id{$_} ) }
    %timer_id = ();
    $screen->clearGamePieces();
    if ( !$gameover ) { $screen->clearMessages() }
    $screen->showGuys( $rocket, $guys );
    $rocket->makeRocket();

    # Autopilot the ship if the game is over
    if ( $gameover || $cheat ) {
        calcAutopilot();
    }
    else {
        $autopilot = 0;
    }

    tick();

    return;
}

# Subroutine: pause
# Called to pause the game, either when the pause key is pressed
# or when an option or help window pops up.
sub pause {
    my $pause = shift;
    $pause   ||= '';
    $paused  ||= 0;
    $purpose ||= 0;
    if ( ref $pause ) { $pause = shift }
    if ( !$gameover ) {
        if ( ( $paused && $pause eq '' ) || ( $pause eq 'off' && !$purpose ) ) {
            $paused   = 0;
            $pause_cb = 0;
            $purpose  = 0;
            $screen->clearMessages();
        }
        else {
            $paused   = 1;
            $pause_cb = 1;
            $purpose  = ( $pause eq '' || $purpose ) ? 1 : 0;
            $screen->showPause();
        }
    }

    return;
}

# Subroutine: playSound
# Plays the specified sound. (Win32 only)
sub playSound {
    my $sound = shift;
    if ( !$nosound && $config{sound} && !$gameover ) {
        Win32::Sound::Play( "$ROCKET_DIR/$sound", &Win32::Sound::SND_ASYNC );
    }

    return;
}

# Subroutine: r
# Return a random integer from 1 to the first parameter.
sub r {
    my $num = shift;
    return int( rand($num) + 1 );
}

# Subroutine: readConfig
# Read the configuration file
sub readConfig {
    open my $INFILE, '<', "$ROCKET_DIR/$INI_FILE" or croak "Can't open ini file: $!";
    while (<$INFILE>) {
        chomp;
        s/\s//xg;
        my ( $key, $value ) = split /=/x;
        $config{$key} = $value;
    }
    close $INFILE;

    return;
}

# Subroutine: setBaseLevel
# This is called to set the difficulty level of the game
# by adjusting the base.
sub setBaseLevel {
    my $level = shift;
    $base->setWidth( $width[$level] );
    $base->make( $b_colors[$level] )
      ;    #ZZZ all of this stuff should be contained in Base
    $base->setIncrement( $b_increments[$level] );

    return;
}

# Subroutine: setHigh
# set the high score
sub setHigh {
    my $h = shift;
    $config{high} = $h;
    writeConfig();

    return;
}

# Subroutine: setupWindow
# Create the main window
sub setupWindow {
    $MW = MainWindow->new;
    $MW->title("Rocket");
    $frame = $MW->Frame(
        -relief      => 'ridge',
        -borderwidth => 2
      )->pack(
        -side   => 'top',
        -anchor => 'n',
        -fill   => 'x'
      );
    $menu = $frame->Menubutton(
        -text      => "Rocket",
        -underline => 0,
        -font      => $FONT,
        -tearoff   => 0,
        -menuitems => [
            [
                'command'  => " New Game (n)",
                -underline => 1,
                -font      => $FONT,
                -command   => \&startGame
            ],
            [
                'checkbutton' => " Pause (p)",
                -underline    => 1,
                -onvalue      => 1,
                -offvalue     => 0,
                -variable     => \$pause_cb,
                -command      => \&pause,
                -font         => $FONT,
                -command      => \&pause
            ],
            [
                'command'  => " Options",
                -underline => 1,
                -font      => $FONT,
                -command   => \&showOptions
            ],
            [
                'command'  => " Exit (x)",
                -underline => 2,
                -font      => $FONT,
                -command => sub { exit }
            ]
        ]
    )->pack( -side => 'left' );
    $hmenu = $frame->Menubutton(
        -text      => "Help",
        -underline => 0,
        -font      => $FONT,
        -tearoff   => 0,
        -menuitems => [
            [
                'command'  => "Help",
                -underline => 0,
                -font      => $FONT,
                -command   => \&showHelp
            ],
            [
                'command'  => "About",
                -underline => 0,
                -font      => $FONT,
                -command   => \&showAbout
            ]
        ]
    )->pack( -side => 'left' );

    return;
}

# Subroutine: startGame
# Initializes the game.  Called when a new game is started.
sub startGame {
    $gameover = shift;
    if ( ref $gameover ) { $gameover = shift }
    if ( !$gameover ) {
        $score = 0;
        $level = 0;
    }
    $guys    = $MAX_GUYS;
    $purpose = 0;
    pause('off');
    $screen->showScores( $score, $config{high} );
    $screen->showGuys( $rocket, $guys );
    setBaseLevel($level);
    init();

    return;
}

# Subroutine: showAbout
# Called to bring up the About window.
sub showAbout {
    my $howtouse_d = $MW->Dialog(
        -text           => "Rocket\nversion $VERSION\n$COPYRIGHT\n\n",
        -title          => 'About',
        -font           => $FONT,
        -default_button => 'Ok',
        -buttons        => ['Ok']
    );
    pause('on');
    $howtouse_d->geometry('480x160');
    $howtouse_d->Show;
    pause('off');

    return;
}

# Subroutine: showHelp
# Called to bring up the Help window.
sub showHelp {
    my $help_d;
    my $text = <<"EOF";
    Rocket
    version $VERSION\n$COPYRIGHT

    Land the rocket on the moving platform.
    Use the left, right, and down arrows to navigate.
    Other keys:
    p - pause
    x - exit
    n - new game

    There is no "thrust".  It's part of the challenge.


EOF
    $text =~ s/[ ][ ][ ][ ]//xg;
    $help_d = $MW->Dialog(
        -text           => $text,
        -title          => 'Help',
        -font           => $FONT,
        -default_button => 'Ok',
        -buttons        => ['Ok']
    );
    pause('on');
    $help_d->Show;
    pause('off');

    return;
}

# Subroutine: showOptions
# Called to bring up the Options window.
sub showOptions {
    my $option_d = $MW->Toplevel();
    my $state = 'normal';
    my $subok = sub {
        $option_d->destroy;
        $config{sound} = $sound_c;
        writeConfig();
        pause('off');
    };
    if ($nosound) {
        $state   = 'disabled';
        $sound_c = 0;
    }

    pause('on');
    $option_d->geometry('130x80');
    $option_d->grab();
    $option_d->bind( '<Return>' => $subok );
    $option_d->bind( '<Alt-Key-p>' => sub { $sound_c = $sound_c ? 0 : 1 } );
    $option_d->title("Options");
    $option_d->focus;

    my $undoc_cb = $option_d->Checkbutton(
        -text      => "Play sound",
        -underline => 0,
        -variable  => \$sound_c,
        -font      => $FONT,
        -state     => $state
      )->pack(
        -side => 'top',
        -padx => $PADX,
        -pady => $PADY
      );
    my $ok = $option_d->Button(
        -text    => 'OK',
        -font    => $FONT,
        -width   => $BWIDTH,
        -command => $subok
    )->pack( -side => 'top' );
    $option_d->waitWindow();

    return;
}

# Subroutine: success
# This is called when a successful landing on the base is detected.
sub success {
    if ( !$gameover ) {
        $score += $SUCCESS_POINTS;
        if ( $score > $config{high} ) {
            setHigh($score);
        }
    }
    $screen->showScores( $score, $config{high} );
    if ( $gameover && r($BASE_RAND) == 1 ) {
        setBaseLevel( r( $#b_increments + 1 ) - 1 );
    }
    $score ||= 0;
    if ( $score >= $next_level[$level] && $level < $#b_increments ) {
        setBaseLevel( ++$level );
    }
    if ( !( $score % $EXTRA_GUY ) && !$gameover ) {
        ++$guys;
        $screen->showGuys( $rocket, $guys );
        playSound($EXTRA_GUY_SOUND);
    }
    else {
        playSound($LANDING_SOUND);
    }
    $anim_seq       = 1;
    $success_frames = r($SUCCESS_FRAME_PLAY) + $SUCCESS_FRAME_MIN;
    successAnim();

    return;
}

# Subroutine: successAnim
# Called by success, this routine is like the "tick" routine for the
# success animation (that period of time where the rocket is just going
# back and forth on the platform after a landing).
sub successAnim {
    if ( !$paused ) {
        ++$anim_seq;
        if ( $anim_seq <= $success_frames ) {

            # strictly speaking in OO terms, this is cheating.  The base
            # shouldn't know anything about the rocket, but it's much
            # more convenient and efficient to use the canvas methods
            # to move the rocket with the base.  I refuse to feel guilty! :)
            $base->move('base_and_rocket');
            $timer_id{t} = $MW->after( $TICK_DELAY, \&successAnim );
        }
        else {
            init();
        }
    }
    else {
        $timer_id{t} = $MW->after( $TICK_DELAY, \&successAnim );
    }

    return;
}

# Subroutine: writeConfig
# Write the configuration back to the config file
sub writeConfig {
    open my $OUTFILE, '>', "$ROCKET_DIR/$INI_FILE" or croak "Can't open ini file: $!";
    for ( keys %config ) {
        print $OUTFILE "$_ = $config{$_}\n";
    }
    close $OUTFILE;

    return;
}

__END__

=head1 NAME

Rocket

=head1 DESCRIPTION

This is a game, the object of which is to land a rocket on a moving platform.

=head1 README

Unzip the archive into its own directory.  cd to the directory.

Usage:

perl rocket.pl

You fly the rocket with the left, right, and down keys.
Other keys:
<p>  - pause
<x>  - exit
<n>  - new game
<up> - God mode?

=head1 PREREQUISITES

This script requires C<Tk>, C<Tk::Dialog>, C<Tk::JPEG>, and C<Win32::Sound> (for sound under Win32 - no sound support
for Unix).

=head1 OSNAMES

Win32, Unix

=head1 SCRIPT CATEGORIES

Win32
Games

=head1 VERSION

1.03

=head1 HISTORY

Version 1.03
    - Sacrifice constants for speed
Version 1.02
    - Cleaned up code
Version 1.01
    - Added autopilot AI
    - Cleaned up code
Version 1.00
    - Finally stable (whew)

=head1 AUTHOR

David Bradford davembradford@gmail.com

=head1 COPYRIGHT

Copyright (c) 2003 David Bradford.  All rights reserved.  This program is free software;
you can redistribute it and/or modify it under the same terms as Perl itself; however, you
must leave this copyright statement intact.

=head1 DATE

May 1, 2003

=head1 SOURCE

This distribution can also be found at the author's web site

http://www.tinypig.com

=cut
