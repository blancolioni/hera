private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

with Hera.Calendar;
with Hera.Knowledge;
with Hera.Money;
with Hera.Objects;
with Hera.Json;
with Hera.Serialization;

package Hera.Accounts is

   type Root_Transaction_Type is abstract tagged private;

   function Class_Name
     (Transaction : Root_Transaction_Type)
      return String
      is abstract;

   function Description
     (Transaction : Root_Transaction_Type)
      return String
      is abstract;

   type Transaction_Type is access constant Root_Transaction_Type'Class;

   function Amount
     (Transaction : Root_Transaction_Type'Class)
      return Hera.Money.Money_Type;

   type Root_Account_Type is
     new Hera.Objects.Root_Hera_Object with private;

   procedure Iterate_Transactions
     (Account : Root_Account_Type'Class;
      Process : not null access
        procedure (Transaction : Transaction_Type));

   type Account_Type is access constant Root_Account_Type'Class;

   procedure New_Transaction
     (Transaction : in out Root_Transaction_Type'Class;
      From, To    : Account_Type;
      Amount      : Hera.Money.Money_Type);

   function Current_Cash
     (Account : Root_Account_Type'Class)
      return Hera.Money.Money_Type;

   procedure Move_Cash
     (From        : Root_Account_Type'Class;
      To          : not null access constant Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Proportion  : Unit_Real);

   procedure Spend
     (Account     : Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Amount      : Hera.Money.Money_Type);

   procedure Earn
     (Account     : Root_Account_Type'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Amount      : Hera.Money.Money_Type);

   function New_Account
     (Name       : String;
      Start_Cash : Hera.Money.Money_Type)
      return Account_Type;

   procedure Iterate_Accounts
     (Process : not null access
        procedure (Account : Account_Type));

   type Has_Account_Interface is interface;

   function Account
     (Item : Has_Account_Interface)
      return Account_Type is abstract;

   function Cash
     (Item : Has_Account_Interface'Class)
      return Hera.Money.Money_Type
   is (Item.Account.Current_Cash);

   procedure Spend
     (Has_Account : Has_Account_Interface'Class;
      Transaction : not null access constant Root_Transaction_Type'Class;
      Amount      : Hera.Money.Money_Type);

   procedure Earn
     (Has_Account  : Has_Account_Interface'Class;
      Transaction  : not null access constant Root_Transaction_Type'Class;
      Amount       : Hera.Money.Money_Type);

   type Transaction_Summary is private;

   function Get_Transaction_Summary
     (Account : Root_Account_Type'Class;
      From, To : Hera.Calendar.Time)
      return Transaction_Summary;

   procedure Iterate
     (Summary : Transaction_Summary;
      Process : not null access
        procedure (Class : String;
                   Amount : Hera.Money.Money_Type));

private

   type Root_Transaction_Type is abstract tagged
      record
         Time   : Hera.Calendar.Time;
         Amount : Hera.Money.Money_Type;
         From   : Account_Type;
         To     : Account_Type;
      end record;

   function Amount
     (Transaction : Root_Transaction_Type'Class)
      return Hera.Money.Money_Type
   is (Transaction.Amount);

   package Transaction_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Transaction_Type);

   type Root_Account_Type is
     new Hera.Objects.Root_Hera_Object with
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Cash         : Hera.Money.Money_Type;
         Transactions : Transaction_Lists.List;
      end record;

   overriding function Log_Id
     (Account : Root_Account_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Account.Name));

   overriding procedure Serialize
     (Account         : Root_Account_Type;
      Detail          : Hera.Serialization.Detail_Level_Type;
      Knowledge       : Hera.Knowledge.Knowledge_Interface'Class;
      Json            : in out Hera.Json.Json_Object'Class);

   function Current_Cash
     (Account : Root_Account_Type'Class)
      return Hera.Money.Money_Type
   is (Account.Cash);

   package Summary_Maps is
     new WL.String_Maps (Hera.Money.Money_Type, Hera.Money."=");

   type Transaction_Summary is
      record
         Map : Summary_Maps.Map;
      end record;

end Hera.Accounts;
