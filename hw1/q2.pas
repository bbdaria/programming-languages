program q2;

type
  TLetterSet = set of 'a'..'z';
  TFrequencyArray = array['a'..'z'] of Integer;

var
  Word1, Word2, Word3: string;
  Letters1, Letters2, Letters3, Letters12, Letters2o, Letters3o: TLetterSet;
  Ch: Char;
  i: Integer;
  Frequency: TFrequencyArray;

begin
  Letters1 := [];
  Letters2 := [];
  Letters3 := [];
  Letters12 := [];
  Letters2o := [];
  Letters3o := [];

  ReadLn(Word1);
  ReadLn(Word2);
  ReadLn(Word3);

  for i := 1 to Length(Word1) do
  begin
    Ch := Word1[i];
    if Ch in ['a'..'z'] then
      Letters1 := Letters1 + [Ch];
  end;

  for i := 1 to Length(Word2) do
  begin
    Ch := Word2[i];
    if Ch in ['a'..'z'] then
      Letters2 := Letters2 + [Ch];
  end;

  for i := 1 to Length(Word3) do
  begin
    Ch := Word3[i];
    if Ch in ['a'..'z'] then
      Letters3 := Letters3 + [Ch];
  end;

  Letters12 := Letters1 >< Letters2;
  Letters2o := Letters2 - Letters3;
  Letters3o := Letters3 - Letters2;

  if (Letters3 <= Letters12) and not (Letters3 <= Letters2o) and not (Letters3 <= Letters3o) then
    WriteLn('TRUE')
  else
    WriteLn('FALSE');

  for Ch := 'a' to 'z' do
    Frequency[Ch] := 0;

  for i := 1 to Length(Word1) do
  begin
    Ch := Word1[i];
    if Ch in ['a'..'z'] then
      Inc(Frequency[Ch]);
  end;

  for i := 1 to Length(Word2) do
  begin
    Ch := Word2[i];
    if Ch in ['a'..'z'] then
      Inc(Frequency[Ch]);
  end;

  for i := 1 to Length(Word3) do
  begin
    Ch := Word3[i];
    if Ch in ['a'..'z'] then
      Inc(Frequency[Ch]);
  end;

  for Ch := 'a' to 'z' do
  begin
    if Frequency[Ch] > 0 then
      WriteLn(Ch, ': ', Frequency[Ch]);
  end;
end.
