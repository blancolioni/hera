private with Lui.Colors;
private with Hera.Color;

with Lui.Models;

package Hera.UI.Models is

   type Root_Hera_Model is
     abstract new Lui.Models.Root_Object_Model with private;

   type Hera_Model is access all Root_Hera_Model'Class;

private

   type Root_Hera_Model is
     abstract new Lui.Models.Root_Object_Model with
      record
         null;
      end record;

   function To_Lui_Color
     (Color : Hera.Color.Hera_Color)
      return Lui.Colors.Color_Type;

   function Have_Model
     (Key     : String)
      return Boolean;

   function Get_Model
     (Key     : String)
      return Lui.Models.Object_Model
     with Pre => Have_Model (Key);

   procedure Save_Model
     (Model    : not null access Lui.Models.Root_Object_Model'Class;
      Class_Id : String);

end Hera.UI.Models;
