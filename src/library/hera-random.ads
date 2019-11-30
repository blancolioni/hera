package Hera.Random is

   procedure Reset;
   procedure Reset (Initiator : Integer);

   function Unit_Random return Unit_Real;

   function Normal_Random
     (Standard_Deviation : Non_Negative_Real)
      return Real;

   function Integer_Random (Low, High : Integer) return Integer;

   function About
     (Value     : Real;
      Variation : Real)
      return Real;

   function D6 return Non_Negative_Real
   is (Unit_Random * 5.0 + 1.0);

   function D (Count : Positive) return Non_Negative_Real;

   subtype Die_Roll is Integer range 1 .. 6;
   subtype Double_Roll is Integer range 2 .. 12;
   subtype Triple_Roll is Integer range 3 .. 18;

   function D6 return Die_Roll
   is (Integer_Random (1, 6));

   function DR return Double_Roll
   is (D6 + D6);

   function TDR return Triple_Roll
   is (D6 + DR);

end Hera.Random;
