with Hera.Money;

package Hera.Reports is

   procedure Start_Report (Name : String);

   procedure Heading
     (Text : String;
      Width : Positive);

   procedure Data (Text : String);
   procedure Data (Value : Integer);
   procedure Data (Value : Real);
   procedure Data (Value : Boolean);
   procedure Data (Value : Hera.Money.Money_Type);

   procedure End_Report;

end Hera.Reports;
