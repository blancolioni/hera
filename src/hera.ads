package Hera is

   pragma Pure (Hera);

   type Quality_Type is range 1 .. 3;

   subtype Real is Long_Float range Long_Float'First .. Long_Float'Last;

   subtype Unit_Real is Real range 0.0 .. 1.0;
   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   function Clamp (X : Real;
                   Lo, Hi : Real)
                   return Real
   is (Real'Max (Lo, Real'Min (Hi, X)));

   function Unit_Clamp (X : Real) return Unit_Real
   is (Clamp (X, 0.0, 1.0));

   function Signed_Unit_Clamp (X : Real) return Signed_Unit_Real
   is (Clamp (X, -1.0, 1.0));

end Hera;
