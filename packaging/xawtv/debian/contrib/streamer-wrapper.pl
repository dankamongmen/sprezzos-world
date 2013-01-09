#!/usr/bin/perl -w

use Getopt::Long;

# $FAKE = 1;
$FRAMESIZE = "320x240";
$HIGHRESVAL = "720x480";
$FPS = 30;
$ENCODING = "mjpeg";

GetOptions("minutes=i" => \$MINUTES,
	   "fps=i" => \$FPS,
	   "outfile=s" => \$OUTFILE,
	   "framesize=s" => \$FRAMESIZE,
	   "channel=i" => \$CHANNEL,
	   "mute" => \$MUTE,
	   "highres" => \$HIGHRES,
	   "encoding" => \$ENCODING);

validateOpts();

runProgram();

sub validateOpts {
  synerror() unless $MINUTES;
  synerror() unless $FPS;
  synerror() unless $OUTFILE;
  synerror() unless $CHANNEL;
  if ($HIGHRES) {
    $FRAMESIZE = $HIGHRESVAL;
  }
}

sub synerror {
  print <<EOF;
There was an error with the command.  Options are:

  --minutes=30         Amount of time to record.  MANDATORY.
  --fps=30             Frames per second of recording, default to $FPS.
  --outfile=foo.avi    Name of output file.  MANDATORY.
  --framesize=320x240  Size of each frame, default to $FRAMESIZE.
  --channel=7          TV channel from which to record.  MANDATORY.
  --highres            Sets FRAMESIZE to $HIGHRESVAL.  BROKEN, DON'T USE
  --mute               Disables audio recording.
  --encoding=mjpeg     Sets encoding type.  Default: $ENCODING

EOF
  die "Invalid syntax.";
}

sub runProgram {
  print " *** Ready to record $MINUTES minutes of video from channel\n";
  print " *** $CHANNEL at $FPS fps to file $OUTFILE";
  if ($MUTE) {
    print (" without sound");
  } else {
    print " with sound";
  }
  print "\n *** The frame size is $FRAMESIZE.";

  print "\n\n";


  print " *** Setting channel...\n";
  mysystem("v4lctl setchannel $CHANNEL");
  
  print " *** Invoking record...\n";

  mysystem("streamer " .
	   ($MUTE ? " " : "-a m ") .
	   "-f $ENCODING -n ntsc -s $FRAMESIZE -t " .
	   ($MINUTES * $FPS * 60) .
	   " -r $FPS -o $OUTFILE");
}

sub mysystem {
  my $command = shift @_;
  print " *** Executing: $command\n";
  if ($FAKE) {
    print " *** Fake mode enabled, actual execution skipped.\n";
    return;
  }

  my $result = system($command) / 256;

  if ($result) {
    print " *** Command returned error $result; ABORTING\n";
    die "Subcommand errored.";
  }
}
