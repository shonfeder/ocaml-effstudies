Handling the read effect by reading real input

  $ ioeff read <<END
  > LeRoi
  > Jones
  warning: here-document at line 1 delimited by end-of-file (wanted `END')
  What is your first name?
  What is your last name?
  LeRoi Jones

Handling the read effect by reading in constant values

  $ ioeff read-constant
  What is your first name?
  What is your last name?
  FOO FOO

Standard handler for an echo computation

  $ ioeff echo <<END
  > first
  > second
  > third
  > END
  first
  second
  third

Reversed print handler for an echo computation

  $ ioeff echo-reverse <<END
  > first
  > second
  > third
  > END
  third
  second
  first

Using a reversed, output collecting handler to consolodate all echos

  $ ioeff echo-collect <<END
  > first
  > second
  > third
  > END
  third, second, first

Using the parameter-passing version of the output collector

  $ ioeff echo-collectp <<END
  > first
  > second
  > third
  > END
  first, second, third
