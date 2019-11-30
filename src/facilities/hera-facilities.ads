private with Ada.Containers.Doubly_Linked_Lists;
private with Tropos;

with Hera.Money;
with Hera.Quantities;

with Hera.Objects;

with Hera.Commodities;
with Hera.Pops.Classes;

package Hera.Facilities is

   type Facility_Class is
     (Colony_Hub, Orbital_Dock,
      Factory, Extractor, Service);

   type Root_Facility_Type (Class : Facility_Class) is
     new Hera.Objects.Root_Localised_Object with private;

   type Facility_Type is access constant Root_Facility_Type'Class;

   function Capacity
     (Facility : Root_Facility_Type'Class)
      return Hera.Quantities.Quantity_Type;

   function Power
     (Facility : Root_Facility_Type'Class)
      return Hera.Quantities.Quantity_Type;

   function Service_Token
     (Facility : Root_Facility_Type'Class)
      return Hera.Commodities.Commodity_Type
     with Pre => Facility.Class = Service;

   function Service_Charge
     (Facility : Root_Facility_Type'Class)
      return Hera.Money.Price_Type
     with Pre => Facility.Class in Colony_Hub | Service;

   function Modules
     (Facility : Root_Facility_Type)
      return Hera.Commodities.Commodity_Array;

   function Quantity
     (Facility : Root_Facility_Type;
      Module   : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type;

   function Workers
     (Facility : Root_Facility_Type)
      return Hera.Pops.Classes.Pop_Class_Array;

   function Quantity
     (Facility  : Root_Facility_Type;
      Worker    : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Quantities.Quantity_Type;

   function Produces_Class
     (Facility : Root_Facility_Type;
      Class    : Hera.Commodities.Commodity_Class)
      return Boolean;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Facility_Type
     with Pre => Exists (Tag);

   type Facility_Array is array (Positive range <>) of Facility_Type;

   function Get_Extractors
     (Resource : Hera.Commodities.Commodity_Type)
      return Facility_Array;

   function Get_Services
     (Quality : Quality_Type)
      return Facility_Array;

   type Production_Item is
      record
         Commodity : Hera.Commodities.Commodity_Type;
         Quantity  : Hera.Quantities.Quantity_Type;
      end record;

   type Production_Queue is tagged private;

   function Has_Queued_Production
     (Queue : Production_Queue)
      return Boolean;

   function Next_Production
     (Queue : Production_Queue)
      return Production_Item;

   procedure Delete_First
     (Queue : in out Production_Queue);

   procedure Iterate
     (Queue : Production_Queue;
      Process : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type));

   procedure Append
     (Queue : in out Production_Queue;
      Item  : Production_Item);

   procedure Insert
     (Queue    : in out Production_Queue;
      Priority : Natural;
      Item     : Production_Item);

   function Changed
     (Queue : Production_Queue)
      return Boolean;

   procedure Clear_Changed
     (Queue : in out Production_Queue);

   procedure Clear
     (Queue : in out Production_Queue);

private

   type Facility_Module is
      record
         Module   : Hera.Commodities.Commodity_Type;
         Quantity : Hera.Quantities.Quantity_Type;
      end record;

   package Facility_Module_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Facility_Module);

   type Facility_Worker is
      record
         Pop_Class : Hera.Pops.Classes.Pop_Class_Type;
         Quantity  : Hera.Quantities.Quantity_Type;
      end record;

   package Facility_Worker_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Facility_Worker);

   type Factory_Production is
     array (Hera.Commodities.Commodity_Class) of Boolean;

   type Resource_Extraction is
     array (Hera.Commodities.Resource_Class) of Boolean;

   type Root_Facility_Type
     (Class : Facility_Class)
   is new Hera.Objects.Root_Localised_Object with
      record
         Power          : Hera.Quantities.Quantity_Type;
         Capacity       : Hera.Quantities.Quantity_Type;
         Service_Charge : Hera.Money.Price_Type;
         Modules        : Facility_Module_Lists.List;
         Employees      : Facility_Worker_Lists.List;
         case Class is
            when Colony_Hub =>
               null;
            when Orbital_Dock =>
               null;
            when Factory =>
               Production : Factory_Production;
            when Extractor =>
               Resources  : Resource_Extraction;
            when Service =>
               Service    : Hera.Commodities.Commodity_Type;
         end case;
      end record;

   overriding procedure Save
     (Facility : Root_Facility_Type;
      To        : in out Tropos.Configuration);

   overriding procedure Load
     (Facility : in out Root_Facility_Type;
      From      : Tropos.Configuration);

   procedure New_Facility
     (Facility : Facility_Type);

   function Capacity
     (Facility : Root_Facility_Type'Class)
      return Hera.Quantities.Quantity_Type
   is (Facility.Capacity);

   function Power
     (Facility : Root_Facility_Type'Class)
      return Hera.Quantities.Quantity_Type
   is (Facility.Power);

   function Service_Token
     (Facility : Root_Facility_Type'Class)
      return Hera.Commodities.Commodity_Type
   is (Facility.Service);

   function Service_Charge
     (Facility : Root_Facility_Type'Class)
      return Hera.Money.Price_Type
   is (Facility.Service_Charge);

   function Get_Class (Class : Facility_Class) return Facility_Array;

   function Get_Services
     (Quality : Quality_Type)
      return Facility_Array
   is (Get_Class (Service));

   type Queue_Entry is
      record
         Item     : Production_Item;
         Priority : Natural;
      end record;

   package Production_Queues is
     new Ada.Containers.Doubly_Linked_Lists (Queue_Entry);

   type Production_Queue is tagged
      record
         Queue   : Production_Queues.List;
         Changed : Boolean := False;
      end record;

   function Has_Queued_Production
     (Queue : Production_Queue)
      return Boolean
   is (not Queue.Queue.Is_Empty);

   function Next_Production
     (Queue : Production_Queue)
      return Production_Item
   is (Queue.Queue.First_Element.Item);

   function Changed
     (Queue : Production_Queue)
      return Boolean
   is (Queue.Changed);

end Hera.Facilities;
