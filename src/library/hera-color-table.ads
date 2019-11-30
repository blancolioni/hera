private package Hera.Color.Table is

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Hera_Color;
   procedure Add (Name : String;
                  Color : Hera_Color);

end Hera.Color.Table;
