with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Text_IO, Ada.Sequential_IO;
use  Ada.Text_IO;

package body MD5.Driver is

  --====================================================================
  -- Authors   Rolf Ebert <Rolf.Ebert@waporo.muc.de>,
  --           Christoph Grein <Christ-Usch.Grein@T-Online.de>
  -- Version   1.3
  -- Date      5 February 1999
  --====================================================================
  -- This is a direct translation into Ada of the C language Reference
  -- Implementation given in the official MD5 algorithm description.
  -- It was originally written by Rolf Ebert.
  -- [See parent package for a reference to the official description.]
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  R.E.    1.0  04.06.1997 Original as found in internet
  --  C.G.    1.1  16.01.1999 Implemented Digest_File and Filter;
  --                          bug fix in Time_Trial; minor code changes;
  --                          commented to make publication legal
  --  C.G.    1.2  20.01.1999 Use Float to output Speed in Time_Trial
  --                          (range of Duration may not be big enough)
  --  C.G.    1.3  05.02.1999 Added Rolf Ebert's address 
  --====================================================================

  procedure Digest_String (Text: in String) is

    C : Context;
    FP: Fingerprint;

  begin

    Init   (C);
    Update (C, Text);
    Final  (C, FP);

    Put ("MD5 (""" & Text & """) = " & Digest_To_Text (FP));
    New_Line;

  end Digest_String;

  ----------------------------------------------------------------------

  procedure Digest_File (Filename: in String; Digest: out String) is

    package Byte_Sequential_IO is new Ada.Sequential_IO (Byte);
    use     Byte_Sequential_IO;

    File: Byte_Sequential_IO.File_Type;

    Contents: Byte_Array (1 .. 1024);
    Last    : Long_Integer := Contents'Last;

    C : Context;
    FP: Fingerprint;

  begin

    Open (File, In_File, Filename);

    Init (C);

    while not End_of_File (File) loop

      for I in Contents'Range loop
        Read (File, Contents (I));
        if End_of_File (File) then
          Last := I;
          exit;
        end if;
      end loop;

      Update (C, Contents (1 .. Last));

    end loop;

    Final (C, FP);

    Close (File);
      Digest := Digest_To_Text(FP);
    --Put_Line ("MD5 (" & Filename & ") = " & Digest_To_Text (FP));

  exception
    when Byte_Sequential_IO.Name_Error =>
      Put_Line(Filename & " can't be opened");

  end Digest_File;

  ----------------------------------------------------------------------

  procedure Filter is

    Contents: String (1 .. 16);
    Last    : constant Natural := Contents'Last;

    C : Context;
    FP: Fingerprint;

  begin

    Init (C);

    Whole_File: loop

      for I in Contents'Range loop
        begin
          Get_Immediate (Contents (I));  -- for control characters
        exception  -- End_Of_File cannot be used for Standard_Input
          when End_Error =>
            Update (C, Contents (1 .. I - 1));
            exit Whole_File;
        end;
      end loop;

      Update (C, Contents (1 .. Last));

    end loop Whole_File;

    Final (C, FP);

    Put_Line (Digest_To_Text (FP));

  end Filter;

  ----------------------------------------------------------------------

  procedure Time_Trial is

    use Ada.Calendar;

    C  : Context;
    FP : Fingerprint;

    Test_Block_Len  : constant :=  5_000;
    Test_Block_Count: constant := 10_000;

    Block: Byte_Array (1 .. Test_Block_Len);

    Start, Stop: Time;

  begin

    Put ("MD5 time trial. Digesting" & Integer'Image (Test_Block_Count) &
         Integer'Image (Test_Block_Len) & "-byte blocks ...");

    -- Initialize block
    for I in Block'range loop
      Block (I) := Byte (I rem (16#FF# + 1));
    end loop;

    -- Start timer
    Start := Clock;

    -- Digest blocks
    Init (C);
    for I in 1 .. Test_Block_Count loop
      Update (C, Block);
    end loop;
    Final (C, Fp);

    -- Stop timer
    Stop := Clock;

    Put_Line (" done");
    Put_Line ("Digest = " & Digest_To_Text (Fp));
    Put_Line ("Time = " & Duration'Image (Stop - Start) & " seconds");
    Put_Line ("Speed = " & Float'Image (Float (Test_Block_Len * Test_Block_Count) / Float (Stop - Start)) &
              " bytes/second");

  end Time_Trial;

  ----------------------------------------------------------------------

  procedure Test_Suite is
    -- The expected results are:
    -- MD5 ("") = d41d8cd98f00b204e9800998ecf8427e
    -- MD5 ("a") = 0cc175b9c0f1b6a831c399e269772661
    -- MD5 ("abc") = 900150983cd24fb0d6963f7d28e17f72
    -- MD5 ("message digest") = f96b697d7cb7938d525a2f31aaf161d0
    -- MD5 ("abcdefghijklmnopqrstuvwxyz") =
    --          c3fcd3d76192e4007dfb496cca67e13b
    -- MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
    --      "abcdefghijklmnopqrstuvwxyz0123456789") = 
    --          d174ab98d277d9f5a5611c2c9f419d9f
    -- MD5 ("1234567890123456789012345678901234567890" &
    --      "1234567890123456789012345678901234567890") =
    --          57edf4a22be3c955ac49da2e2107b67a
  begin
    Put_Line ("MD5 test suite:");
    Digest_String ("");
    Digest_String ("a");
    Digest_String ("abc");
    Digest_String ("message digest");
    Digest_String ("abcdefghijklmnopqrstuvwxyz");
    Digest_String ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                   "abcdefghijklmnopqrstuvwxyz0123456789");
    Digest_String ("1234567890123456789012345678901234567890" &
                   "1234567890123456789012345678901234567890");
  end Test_Suite;

end MD5.Driver;
