private with Ada.Containers.Doubly_Linked_Lists;
private with Tropos;

with Hera.Identifiers;
with Hera.Money;
with Hera.Quantities;

with Hera.Objects;

package Hera.Commodities is

   type Commodity_Class is
     (Resource, Consumer_Good, Service_Token, Market_Token, Industrial_Good,
      Building_Module, Starship_Component);

   type Resource_Class is
     (Organic, Mineral, Metal, Fissile, Fuel, Gas, Liquid);

   type Consumer_Class is
     (Food, Drink, Intoxicant, Clothing);

   type Service_Class is
     (Education, Entertainment, Fitness, Medical);

   type Industrial_Class is
     (Alloy, Ceramic, Electronic, Plastic, Power, Engine);

   type Building_Module_Class is
     (Structural, Industrial, Commercial, Habitation, Military);

   type Starship_Component_Class is
     (Cargo, Drive, Warp, Scan, Mining, Command, Crew, Probe, Laboratory);

   type Root_Commodity_Type (Class : Commodity_Class) is
     new Hera.Objects.Root_Localised_Object with private;

   function Get_Resource_Class
     (Commodity : Root_Commodity_Type'Class)
      return Resource_Class
     with Pre => Commodity.Class = Resource;

   function Base_Price
     (Commodity : Root_Commodity_Type'Class)
      return Hera.Money.Price_Type;

   function Quality
     (Commodity : Root_Commodity_Type'Class)
      return Quality_Type;

   function Happiness
     (Commodity : Root_Commodity_Type'Class)
      return Unit_Real;

   function Complexity
     (Commodity : Root_Commodity_Type'Class)
      return Hera.Quantities.Quantity_Type;

   type Commodity_Type is access constant Root_Commodity_Type'Class;

   subtype Resource_Type is Commodity_Type
     with Dynamic_Predicate => Resource_Type.Class = Resource;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Commodity_Type
     with Pre => Exists (Tag);

   type Commodity_Array is array (Positive range <>) of Commodity_Type;

   function Resources
     return Commodity_Array;

   function Consumer_Goods
      return Commodity_Array;

   function Services return Commodity_Array;

   function Industrial_Goods
      return Commodity_Array;

   function Building_Modules
     return Commodity_Array;

   function Starship_Components
     return Commodity_Array;

   function Components
     (Commodity : Root_Commodity_Type)
      return Commodity_Array;

   function Quantity
     (Commodity : Root_Commodity_Type;
      Component : Commodity_Type)
      return Hera.Quantities.Quantity_Type;

   type Has_Stock_Interface is interface;

   type Stock_Entry is
      record
         Quantity  : Hera.Quantities.Quantity_Type;
         Value     : Hera.Money.Money_Type;
      end record;

   function Get_Stock
     (From      : Has_Stock_Interface;
      Commodity : Commodity_Type)
      return Stock_Entry
      is abstract;

   procedure Set_Stock
     (To        : Has_Stock_Interface;
      Commodity : Commodity_Type;
      Stock     : Stock_Entry)
   is abstract
     with Pre'Class =>
       (Hera.Quantities."=" (Stock.Quantity, Hera.Quantities.Zero)
        and then Hera.Money."=" (Stock.Value, Hera.Money.Zero))
       or else (Hera.Quantities.">" (Stock.Quantity, Hera.Quantities.Zero)
                and then Hera.Money.">" (Stock.Value, Hera.Money.Zero));

   procedure Update_Stock
     (To        : in out Has_Stock_Interface;
      Commodity : Commodity_Type;
      Stock     : Stock_Entry)
   is abstract;

   procedure Scan_Stock
     (Stock     : Has_Stock_Interface;
      Process   : not null access
        procedure (Commodity : Commodity_Type;
                   Stock : Stock_Entry))
   is abstract;

   function Quantity
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type)
      return Hera.Quantities.Quantity_Type;

   function Price
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type)
      return Hera.Money.Price_Type;

   procedure Add
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Value     : Hera.Money.Money_Type);

   procedure Add
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type);

   procedure Remove
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Value     : out Hera.Money.Money_Type);

   procedure Remove
     (Has_Stock : Has_Stock_Interface'Class;
      Commodity : Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type);

   procedure Replace
     (To   : in out Has_Stock_Interface'Class;
      From : Has_Stock_Interface'Class);

   procedure Scan_Stock_Join
     (Left, Right : Has_Stock_Interface'Class;
      Process     : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Left_Stock, Right_Stock : Stock_Entry));

   type Stock_List is
     new Has_Stock_Interface
     and Hera.Objects.Configured_Interface
   with private;

   procedure Initialize_Stock_List
     (List       : in out Stock_List'Class;
      Identifier : Hera.Identifiers.Object_Identifier);

private

   type Commodity_Component is
      record
         Commodity : Commodity_Type;
         Quantity  : Hera.Quantities.Quantity_Type;
      end record;

   package Commodity_Component_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Commodity_Component);

   type Starship_Component_Type is
      record
         Consumer_Capacity   : Hera.Quantities.Quantity_Type;
         Industrial_Capacity : Hera.Quantities.Quantity_Type;
         Module_Capacity     : Hera.Quantities.Quantity_Type;
         Organic_Capacity    : Hera.Quantities.Quantity_Type;
         Liquid_Gas_Capacity : Hera.Quantities.Quantity_Type;
         Thrust              : Natural;
         Warp                : Natural;
         Long_Range_Scan     : Natural;
         Surface_Scan        : Boolean;
         Asteroid_Mining     : Boolean;
         Gas_Field_Ming      : Boolean;
         Command             : Boolean;
         Crew                : Boolean;
         System_Probe        : Boolean;
         Planet_Probe        : Boolean;
         Bio_Lab             : Boolean;
         Physics_Lab         : Boolean;
         Geo_Lab             : Boolean;
      end record;

   type Root_Commodity_Type
     (Class : Commodity_Class)
   is new Hera.Objects.Root_Localised_Object with
      record
         Mass       : Non_Negative_Real      := 0.0;
         Price      : Hera.Money.Price_Type  := Hera.Money.Zero;
         Transient  : Boolean                := False;
         Happiness  : Unit_Real              := 0.0;
         Components : Commodity_Component_Lists.List;
         case Class is
            when Resource =>
               Resource_Type    : Resource_Class;
            when Consumer_Good =>
               Consumer_Type    : Consumer_Class;
               Consumer_Quality : Quality_Type;
            when Service_Token =>
               Service_Type     : Service_Class;
               Service_Quality  : Quality_Type;
            when Market_Token =>
               null;
            when Industrial_Good =>
               Industrial_Type  : Industrial_Class;
            when Building_Module =>
               Building_Module_Type : Building_Module_Class;
            when Starship_Component =>
               Component            : Starship_Component_Type;
         end case;
      end record;

   overriding procedure Save
     (Commodity : Root_Commodity_Type;
      To        : in out Tropos.Configuration);

   overriding procedure Load
     (Commodity : in out Root_Commodity_Type;
      From      : Tropos.Configuration);

   type Stock_Item is
      record
         Commodity : Commodity_Type;
         Stock     : Stock_Entry;
      end record;

   package Stock_Item_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stock_Item);

   type Stock_List is
     new Has_Stock_Interface
     and Hera.Objects.Configured_Interface with
      record
         Id    : Hera.Identifiers.Object_Identifier;
         Stock : Stock_Item_Lists.List;
      end record;

   overriding function Get_Stock
     (From      : Stock_List;
      Commodity : Commodity_Type)
      return Stock_Entry;

   overriding procedure Scan_Stock
     (List     : Stock_List;
      Process   : not null access
        procedure (Commodity : Commodity_Type;
                   Stock : Stock_Entry));

   overriding procedure Set_Stock
     (List      : Stock_List;
      Commodity : Commodity_Type;
      Stock     : Stock_Entry);

   overriding procedure Update_Stock
     (List      : in out Stock_List;
      Commodity : Commodity_Type;
      Stock     : Stock_Entry);

   overriding procedure Save
     (Stock     : Stock_List;
      To        : in out Tropos.Configuration);

   overriding procedure Load
     (Stock : in out Stock_List;
      From  : Tropos.Configuration);

   procedure New_Commodity
     (Commodity : Commodity_Type);

   function Get_Resource_Class
     (Commodity : Root_Commodity_Type'Class)
      return Resource_Class
   is (Commodity.Resource_Type);

   function Base_Price
     (Commodity : Root_Commodity_Type'Class)
      return Hera.Money.Price_Type
   is (Commodity.Price);

   function Get_Class (Class : Commodity_Class) return Commodity_Array;

   function Quality_Goods
     (Goods   : Commodity_Array;
      Quality : Quality_Type)
      return Commodity_Array;

   function Resources
     return Commodity_Array
   is (Get_Class (Resource));

   function Consumer_Goods
      return Commodity_Array
   is (Get_Class (Consumer_Good));

   function Industrial_Goods
     return Commodity_Array
   is (Get_Class (Industrial_Good));

   function Building_Modules
     return Commodity_Array
   is (Get_Class (Building_Module));

   function Starship_Components
     return Commodity_Array
   is (Get_Class (Starship_Component));

   function Services return Commodity_Array
   is (Get_Class (Service_Token));

   function Quality
     (Commodity : Root_Commodity_Type'Class)
      return Quality_Type
   is (case Commodity.Class is
          when Consumer_Good =>
             Commodity.Consumer_Quality,
          when Service_Token =>
             Commodity.Service_Quality,
          when others        =>
             raise Constraint_Error with
               Commodity.Tag & " has no quality metric");

   function Happiness
     (Commodity : Root_Commodity_Type'Class)
      return Unit_Real
   is (Commodity.Happiness);

end Hera.Commodities;
