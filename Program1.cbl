       identification division.
       program-id. A2-ItemList.
       author. Kaifkhan Vakil.
       Date-written. 2021-01-26.

      *****************************************************************
      *Description: We are going to make a report in such a manner as 
      *the desired output is. First we will use the basic input and 
      *output reading from a file, then we will modify in the way we 
      *want. Then i will add some more columns in it such a extended 
      *price, net price, discount percentage, tranportation charge and 
      *to make them in such a manner that we can display it properly as
      *we want it. I also made the function to add the calculation for 
      *some of the prices like extended price, net price and 
      *transportation charge. Lastly, i will also make it display the 
      *percentage of the items on which teh discount was found.
      *****************************************************************


       environment division.
       configuration section.

       input-output section.
       file-control.
      *We will read from the input file.
           select input-file
           assign to "../../../A2.dat"
           organization is line sequential.

      *We will assign an output file in which we will be assigning the 
      *data
           select output-file
           assign to "../../../A2-ItemList.out"
           organization is line sequential.


       data division.
       file section.

      *Input and output files and record definitions

       fd input-file
           data record is input-line
           record contains 30 characters.

       01 input-line.
         05 il-item-number         pic x(4).
         05 il-product-class       pic x(1).
         05 il-description         pic x(13).
         05 il-quantity            pic 999.
         05 il-unit-price          pic 9(4)v99.

       fd output-file
           data record is output-line
           record contains 200 characters.

       01 output-line              pic x(200).

       working-storage section.

      *****************************************************************
      *Working storage section assigned for the heading for the report,
      *this will show my name along with assignment name.


       01 ws-report-heading.
         05 filler                 pic x(142)    value spaces.
         05 filler                 pic x(18)     value 
         "Kaifkhan Vakil, A2".
         05 filler                 pic x(10)     value spaces.


      *****************************************************************
      *This section is focusing on column heading for a report which 
      *will show the column heading and what the details it will be 
      *showing

       01 ws-column-heading.
         05 filler                 pic x(11)     value "Item Number".
         05 filler                 pic x(2)      value spaces.
         05 filler                 pic x(2)      value spaces.
         05 filler                 pic x(11)     value "Description".
         05 filler                 pic x(6)      value spaces.
         05 filler                 pic x(3)      value "Qty".
         05 filler                 pic x(5)      value spaces.
         05 filler                 pic x(14)     value "Price per unit".
         05 filler                 pic x(6)      value spaces.
         05 filler                 pic x(15)     value "Extended Price".
         05 filler                 pic x(2)      value spaces.
         05 filler                 pic x(17)     value 
         "Discounted Amount".
         05 filler                 pic x(3)      value spaces.
         05 filler                 pic x(9)      value "Net price".
         05 filler                 pic x(3)      value spaces.
         05 filler                 pic x(13)     value "Product Class".
         05 filler                 pic x(2)      value spaces.
         05 filler                 pic x(16)     value 
         "Transportation %".
         05 filler                 pic x(3)      value spaces.
         05 filler                 pic x(21)     value 
         "Transportation Charge".

      *****************************************************************
      *This is the detail line section which takes care of outputting 
      *data which has been gotten from the 
       01 ws-detail-line.
         05 filler                 pic x(3).
         05 ws-item-number         pic x(4).
         05 filler                 pic x(8).
         05 ws-description         pic x(13).
         05 filler                 pic x(5)      value spaces.
         05 ws-quantity            pic zz9.
         05 filler                 pic x(7)      value spaces.
         05 ws-unit-price          pic z,zz9.99.
         05 filler                 pic x(10)     value spaces.
         05 ws-extended-price      pic z,zzz,zz9.99.
         05 filler                 pic x(6)      value spaces.
         05 ws-discounted-price    pic zzz,zz9.99.
         05 filler                 pic x(6)      value spaces.
         05 ws-net-price           pic z,zzz,zz9.99.
         05 filler                 pic x(9)     value spaces.
         05 ws-product-class       pic x(1).
         05 filler                 pic x(12)      value spaces.
         05 ws-trans-percent       pic z9.9.
         05 ws-percent-sign        pic x.
         05 filler                 pic x(10)      value spaces.
         05 ws-trans-charge        pic zzz,zz9.99.
         
       01 ws-summary-line.
         05 filler                 pic x(61)     value spaces.
         05 ws-total-extended      pic $$$,$$$,$$9.99.
         05 filler                 pic x(21)     value spaces.
         05 ws-total-net           pic $$$,$$$,$$9.99.
         05 filler                 pic x(31)     value spaces.
         05 ws-total-trans-charge  pic $$$,$$$,$$9.99.

       01 ws-overall-discount.
         05 ws-discount-total      pic x(22)     value 
       " ITEMS WITHOUT DICOUNT".
         05 ws-equal-sign          pic x(3)      value " = ".
         05 ws-percent-dicount     pic zz9.9     value 0.
         05 ws-discount-sign       pic x.


       01 ws-counters.
         05 ws-price-count     pic 9(9)v99       value 0.
         05 ws-net-count       pic 9(9)v99       value 0.
         05 ws-charge-count    pic 9(9)v99       value 0.
         05 ws-discount-count  pic 999           value 0.
         05 ws-total-items     pic 999           value 0.


       01 ws-cals.
         05 ws-extended-calc       pic 9(8)v99   value 0.
         05 ws-discount-calc       pic 9(8)v99   value 0.
         05 ws-net-price-calc      pic 9(8)v99   value 0.
         05 ws-trans-per-calc      pic 999v9.
         05 ws-trans-charge-calc   pic 9(8)v99   value 0.
         05 ws-interim             pic 999v99    value 0.

       01 ws-constants.
         05 ws-a-class             pic x         value "A".
         05 ws-b-class             pic x         value "B".
         05 ws-f-class             pic x         value "F".
         05 ws-d-class             pic x         value "D".
         05 ws-c-class             pic x         value "C".
         05 ws-z-class             pic x         value "Z".
         05 ws-g-class             pic x         value "G".
         05 ws-percent-sign-cnst   pic x         value "%".




       01 ws-flags.
         05 ws-eof-flag            pic x         value "n".

       procedure division.

       000-main.
      *open files
           open input input-file.
           open output output-file.

      *report heading

           write output-line from ws-report-heading
             after advancing 2 lines.

           write output-line from ws-column-heading
             after advancing 3 lines

      *initial read of input file

           read input-file
               at end
                   move "y" to ws-eof-flag.

      *process each input record and read in the next record

           perform 100-process-file
             until ws-eof-flag equals "y".

           move ws-price-count to ws-total-extended.
           move ws-net-count to ws-total-net.
           move ws-charge-count to ws-total-trans-charge.

           write output-line from ws-summary-line
           after advancing 2 lines.

          divide ws-discount-count by ws-total-items
            giving ws-interim rounded.
           multiply ws-interim by 100
             giving ws-percent-dicount.
           move ws-percent-sign-cnst to ws-discount-sign.

           write output-line from ws-overall-discount
           after advancing 3 lines.

      *Close files and end program
           close input-file.
           close output-file.


           goback.

       100-process-file.

           move 0 to ws-extended-calc.
           move 0 to ws-discount-calc.
           move 0 to ws-net-price-calc.
           move 0 to ws-trans-per-calc.
           move 45 to ws-trans-charge-calc.
           add 1 to ws-total-items.

           multiply il-unit-price by il-quantity
           giving ws-extended-calc rounded.

           if il-product-class = ws-a-class then
               if ws-extended-calc > 100 then
                   multiply ws-extended-calc by 0.05 giving 
                   ws-discount-calc
               end-if
           else 
               if il-product-class = ws-b-class then
                   if ws-extended-calc > 5 then
                       multiply ws-extended-calc by 0.05 giving 
                       ws-discount-calc
                   end-if
               end-if
           end-if.

           if il-product-class = ws-f-class then
               if ws-extended-calc > 50 then
                   multiply ws-extended-calc by 0.05 giving 
                   ws-discount-calc
               end-if
           end-if.

           if il-product-class = ws-a-class then
               move 12.5 to ws-trans-per-calc
               multiply ws-extended-calc by 0.125 giving 
               ws-trans-charge-calc
           end-if.

           if il-product-class = ws-d-class then
               move 8.5 to ws-trans-per-calc
               multiply ws-extended-calc by 0.085 giving 
               ws-trans-charge-calc
           end-if.

           if il-product-class = ws-f-class then
               move 4.5 to ws-trans-per-calc
               multiply ws-extended-calc by 0.045 giving
                 ws-trans-charge-calc
           end-if.

           if (il-product-class = ws-b-class) or (il-product-class = 
           ws-c-class) or (il-product-class = ws-z-class) or 
           (il-product-class = ws-g-class) then
               if il-quantity <= 100 then
                   move 6.5 to ws-trans-per-calc
                   multiply ws-extended-calc by 0.065 giving
                     ws-trans-charge-calc
               end-if
           end-if.

           if ws-discount-calc = 0 then
               add 1 to ws-discount-count
           end-if.

           subtract ws-discount-calc from ws-extended-calc giving 
           ws-net-price-calc.

           add ws-extended-calc to ws-price-count.
           add ws-net-price-calc to ws-net-count.
           add ws-trans-charge-calc to ws-charge-count.

           move spaces to ws-detail-line.

           move il-item-number to ws-item-number.
           move il-product-class to ws-product-class.
           move il-description to ws-description.
           move il-quantity to ws-quantity.
           move il-unit-price to ws-unit-price.
           move ws-extended-calc to ws-extended-price.
           move ws-discount-calc to ws-discounted-price.
           move ws-net-price-calc to ws-net-price.
           move ws-trans-per-calc to ws-trans-percent.
           move ws-percent-sign-cnst to ws-percent-sign.
           move ws-trans-charge-calc to ws-trans-charge.


           write output-line from ws-detail-line
             after advancing 2 lines.

           read input-file
               at end
                   move "y" to ws-eof-flag.

       end program A2-ItemList.