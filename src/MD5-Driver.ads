package MD5.Driver is

  --====================================================================
  -- Authors   Rolf Ebert <Rolf.Ebert@waporo.muc.de>,
  --           Christoph Grein <Christ-Usch.Grein@T-Online.de>
  -- Version   1.2
  -- Date      5 February 1999
  --====================================================================
  -- The Test Driver for the Message-Digest MD5 Algorithm of
  -- RSA Data Security, Inc.
  -- [See parent package for a reference to the official description.]
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  R.E.    1.0  04.06.1997 Original as found in internet
  --  C.G.    1.1  16.01.1999 Commented to make publication legal
  --  C.G.    1.2  05.02.1999 Added Rolf Ebert's address 
  --====================================================================

   procedure Digest_String (Text: in String);

   procedure Digest_File (Filename: in String; Digest: out String);

   procedure Filter;      -- digest standard input

   procedure Time_Trial;  -- performance test

   procedure Test_Suite;  -- test the implementation

end MD5.Driver;
