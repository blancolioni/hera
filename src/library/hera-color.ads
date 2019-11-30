package Hera.Color is

   type Hera_Color is
      record
         Red, Green, Blue, Alpha : Unit_Real;
      end record;

   Black : constant Hera_Color := (0.0, 0.0, 0.0, 1.0);
   White : constant Hera_Color := (1.0, 1.0, 1.0, 1.0);

   function From_String
     (Item : String)
      return Hera_Color;

   function To_Html_String
     (Color : Hera_Color)
      return String;

   function To_Html_String
     (R, G, B : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return String
   is (To_Html_String ((R, G, B, Alpha)));

end Hera.Color;
