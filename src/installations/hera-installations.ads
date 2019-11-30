with Hera.Quantities;

with Hera.Objects;

with Hera.Accounts;
with Hera.Colonies;
with Hera.Commodities;
with Hera.Corporations;
with Hera.Facilities;
with Hera.Sectors;

package Hera.Installations is

   type Root_Installation_Type is
     new Hera.Objects.Root_Hera_Object
     and Hera.Colonies.Employer_Interface
     and Hera.Commodities.Has_Stock_Interface
   with private;

   function Owner
     (Installation : Root_Installation_Type'Class)
      return Hera.Corporations.Corporation_Type;

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Hera.Facilities.Facility_Type;

   function Sector
     (Installation : Root_Installation_Type'Class)
      return Hera.Sectors.Sector_Type;

   function Colony
     (Installation : Root_Installation_Type'Class)
      return Hera.Colonies.Colony_Type;

   type Installation_Type is access constant Root_Installation_Type'Class;

   procedure Clear_Queue
     (Installation : Root_Installation_Type'Class);

   procedure Add_To_Queue
     (Installation : Root_Installation_Type'Class;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Priority     : Natural := 0)
     with Pre => Hera.Quantities.">" (Quantity, Hera.Quantities.Zero);

   function Queued_Capacity
     (Installation : Root_Installation_Type'Class)
      return Hera.Quantities.Quantity_Type;

   procedure Scan_By_Owner
     (Owner : Hera.Corporations.Corporation_Type;
      Process : not null access
        procedure (Installation : Installation_Type));

private

   type Root_Installation_Type is
     new Hera.Objects.Root_Hera_Object
     and Hera.Commodities.Has_Stock_Interface
     and Hera.Colonies.Employer_Interface with
      record
         Owner    : Hera.Corporations.Corporation_Type;
         Colony   : Hera.Colonies.Colony_Type;
         Sector   : Hera.Sectors.Sector_Type;
         Stock    : Hera.Commodities.Stock_List;
         Facility : Hera.Facilities.Facility_Type;
         Queue    : Hera.Facilities.Production_Queue;
      end record;

   overriding function Account
     (Installation : Root_Installation_Type)
      return Hera.Accounts.Account_Type
   is (Installation.Owner.Account);

   overriding function Name
     (Installation : Root_Installation_Type)
      return String
   is (Installation.Facility.Tag & " owned by "
       & Installation.Owner.Name);

   overriding function Get_Stock
     (From      : Root_Installation_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry;

   overriding procedure Set_Stock
     (To        : Root_Installation_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry);

   overriding procedure Scan_Stock
     (List      : Root_Installation_Type;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry));

   overriding procedure Update_Stock
     (Installation : in out Root_Installation_Type;
      Commodity    : Hera.Commodities.Commodity_Type;
      Stock        : Hera.Commodities.Stock_Entry);

   function Owner
     (Installation : Root_Installation_Type'Class)
      return Hera.Corporations.Corporation_Type
   is (Installation.Owner);

   function Facility
     (Installation : Root_Installation_Type'Class)
      return Hera.Facilities.Facility_Type
   is (Installation.Facility);

   function Colony
     (Installation : Root_Installation_Type'Class)
      return Hera.Colonies.Colony_Type
   is (Installation.Colony);

   function Sector
     (Installation : Root_Installation_Type'Class)
      return Hera.Sectors.Sector_Type
   is (Installation.Sector);

   procedure Create
     (Owner    : Hera.Corporations.Corporation_Type;
      Colony   : Hera.Colonies.Colony_Type;
      Facility : Hera.Facilities.Facility_Type);

end Hera.Installations;
