private with Ada.Containers.Ordered_Maps;

package Hera.Color.Palettes is

   type Palette_Type is private;

   function Get_Palette
     (Name : String)
      return Palette_Type;

   function Get_Color
     (Palette : Palette_Type;
      Value   : Real)
      return Hera_Color;

   procedure Load_Palette
     (Name        : String;
      Low, High   : Real;
      Bitmap_Path : String);

private

   package Color_Maps is
     new Ada.Containers.Ordered_Maps (Real, Hera_Color);

   type Palette_Type is
      record
         Map : Color_Maps.Map;
      end record;

end Hera.Color.Palettes;
