private with Tropos;

with Hera.Money;
with Hera.Quantities;

with Hera.Atmosphere;
with Hera.Climate;
with Hera.Commodities;
with Hera.Markets;
with Hera.Star_Systems;
with Hera.Sectors;

package Hera.Planets is

   type Orbit_Zone is
     (Red, Yellow, Green, Blue, Black);

   subtype Planetary_Zone is Orbit_Zone range Yellow .. Black;

   type Atmosphere_Class is (None, Trace, Thin, Standard, Dense);

   type Planet_Composition is
     (Hydrogen, Gaseous, Ice, Rock, Rock_Ice, Rock_Iron);

   subtype Rocky_Planet is Planet_Composition range Ice .. Rock_Iron;

   type Planet_Climate is
     (Airless, Desert, Iceball, Martian, Temperate, Venusian, Water, Jovian);

   type Life_Complexity_Type is
     (None, Prebiotic, Single_Celled, Plants, Multicellular);

   type Planet_Terrain is
     (Ocean, Desert, Ice, Tundra, Mountains);

   type Root_Planet_Type is
     new Hera.Star_Systems.Root_Star_System_Entity with private;

   function Habitability
     (Planet : Root_Planet_Type'Class)
      return Unit_Real;

   function Climate
     (Planet : Root_Planet_Type'Class)
      return Hera.Climate.Climate_Type;

   function Surface_Width
     (Planet : Root_Planet_Type'Class)
      return Natural;

   function Surface_Height
     (Planet : Root_Planet_Type'Class)
      return Natural;

   function Find
     (Planet : Root_Planet_Type'Class;
      Score  : not null access
        function (Sector : Hera.Sectors.Sector_Type)
      return Non_Negative_Real)
      return Hera.Sectors.Sector_Array;

   function Get_Sectors
     (Planet : Root_Planet_Type'Class)
      return Hera.Sectors.Sector_Array;

   procedure On_Production
     (Planet    : Root_Planet_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Cost      : Hera.Money.Money_Type);

   procedure On_Consumption
     (Planet    : Root_Planet_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Cost      : Hera.Money.Money_Type);

   procedure Iterate_Economy
     (Planet  : Root_Planet_Type'Class;
      Process : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Production : Hera.Commodities.Stock_Entry;
                   Consumption : Hera.Commodities.Stock_Entry));

   type Planet_Type is access constant Root_Planet_Type'Class;

   type Planet_Array is array (Positive range <>) of Planet_Type;

   function Find
     (Score : not null access
        function (Planet : Planet_Type)
      return Non_Negative_Real)
      return Planet_Array;

   procedure Iterate
     (Process : not null access
        procedure (Planet : Planet_Type));

private

   type Root_Planet_Type is
     new Hera.Star_Systems.Root_Star_System_Entity with
      record
         Composition         : Planet_Composition;
         Atmosphere          : Hera.Atmosphere.Atmosphere_Type;
         Climate             : Hera.Climate.Climate_Type;
         Gas_Giant           : Boolean;
         Habitability        : Unit_Real;
         Colonized           : Boolean;
         Average_Temperature : Non_Negative_Real;
         Hydrosphere         : Unit_Real;
         Life_Complexity     : Life_Complexity_Type;
         Smoothness          : Natural;
         Elevation_Range     : Natural;
         Sea_Level           : Natural;
         Consumption         : Hera.Markets.Trade_Node_Type;
         Production          : Hera.Markets.Trade_Node_Type;
      end record;

   overriding procedure On_Colonized (Planet : in out Root_Planet_Type);

   overriding procedure Save
     (Object : Root_Planet_Type;
      Config : in out Tropos.Configuration);

   function Habitability
     (Planet : Root_Planet_Type'Class)
      return Unit_Real
   is (Planet.Habitability);

   function Climate
     (Planet : Root_Planet_Type'Class)
      return Hera.Climate.Climate_Type
   is (Planet.Climate);

   procedure New_Planet (Planet : Root_Planet_Type'Class);

end Hera.Planets;
