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
         05 filler                 pic x(84)    value spaces.
         05 filler                 pic x(28)     value 
         "Kaifkhan Vakil, Assignment 2".
         05 filler                 pic x(10)     value spaces.


      *****************************************************************
      *This section is focusing on column heading for a report which 
      *will show the column heading and what the details it will be 
      *showing

       01 ws-title-1.
         05 filler                 pic x(50)     value 
         " ITEM     ITEM       Qty    UNIT       EXTENDED".
         05 filler                 pic x(61)     value 
         
       "       DISCOUNT      NET PRICE  CLASS  TRANS   TRANSPORTATION".

       01 ws-title-2.
         05 filler                 pic x(58)     value 
        "  #    DESCRIPTION          PRICE       PRICE             ".
         05 filler                 pic x(55)     value 
         "AMOUNT                          %          CHARGE    ".

      *****************************************************************
      *This is the detail line section which takes care of outputting 
      *data which has been gotten from the input file.

       01 ws-detail-line.
         05 filler                 pic x(1).
         05 ws-item-number         pic x(4).
         05 filler                 pic x(2).
         05 ws-description         pic x(13).
         05 filler                 pic x(1)      value spaces.
         05 ws-quantity            pic zz9.
         05 filler                 pic x(2)      value spaces.
         05 ws-unit-price          pic z,zz9.99.
         05 filler                 pic x(2)     value spaces.
         05 ws-extended-price      pic z,zzz,zz9.99.
         05 filler                 pic x(6)      value spaces.
         05 ws-discounted-price    pic zzz,zz9.99.
         05 filler                 pic x(4)      value spaces.
         05 ws-net-price           pic z,zzz,zz9.99.
         05 filler                 pic x(4)      value spaces.
         05 ws-product-class       pic x(1).
         05 filler                 pic x(4)     value spaces.
         05 ws-trans-percent       pic z9.9.
         05 ws-percent-sign        pic x.
         05 filler                 pic x(5)     value spaces.
         05 ws-trans-charge        pic z,zzz,zz9.99.
         
      *****************************************************************
      *This is the summary line which will be showing the total of 
      *extended, net and tranportation price

       01 ws-summary-line.
         05 filler                 pic x(34)     value spaces.
         05 ws-total-extended      pic $$$,$$$,$$9.99.
         05 filler                 pic x(18)     value spaces.
         05 ws-total-net           pic $$$,$$$,$$9.99.
         05 filler                 pic x(17)     value spaces.
         05 ws-total-trans-charge  pic $$$,$$$,$$9.99.

      *****************************************************************
      *This section takes care of showing the percentage amount of item 
      *that got the discount on them and which dont.
       01 ws-overall-discount.
         05 ws-discount-total      pic x(22)     value 
       " ITEMS WITHOUT DICOUNT".
         05 ws-equal-sign          pic x(3)      value " = ".
         05 ws-percent-dicount     pic zz9.9     value 0.
         05 ws-discount-sign       pic x.


      *****************************************************************
      *This section is for counting is used for calculation purposes and 
      * if we want to use that count for calculation later.

       01 ws-counters.
         05 ws-price-count        pic 9(9)v99    value 0.
         05 ws-net-count          pic 9(9)v99    value 0.
         05 ws-charge-count       pic 9(9)v99    value 0.
         05 ws-discount-count     pic 999        value 0.
         05 ws-total-items        pic 999        value 0.

      *****************************************************************
      *This section is solely for the purpose of calculation,first we 
      *will conduct calculation in this section then we will move it to 
      *edited clause

       01 ws-calcs.
         05 ws-extended-calc       pic 9(8)v99   value 0.
         05 ws-discount-calc       pic 9(8)v99   value 0.
         05 ws-net-price-calc      pic 9(8)v99   value 0.
         05 ws-trans-per-calc      pic 999v9.
         05 ws-trans-charge-calc   pic 9(8)v99   value 0.
         05 ws-interim             pic 999v99    value 0.

      *****************************************************************
      *This section is of the constants, if those are used in the 
      *calculation.

       01 ws-constants.
         05 ws-a-class             pic x         value "A".
         05 ws-b-class             pic x         value "B".
         05 ws-f-class             pic x         value "F".
         05 ws-d-class             pic x         value "D".
         05 ws-c-class             pic x         value "C".
         05 ws-z-class             pic x         value "Z".
         05 ws-g-class             pic x         value "G".
         05 ws-percent-sign-cnst   pic x         value "%".
         05 ws-100                 pic 999v9     value 100.0.
         05 ws-5-percent           pic 9v99      value 0.05.
         05 ws-5                   pic 9         value 5.
         05 ws-50                  pic 99        value 50.
         05 ws-12-half             pic 99v9      value 12.5.
         05 ws-8-half              pic 9v9       value 8.5.
         05 ws-6-half              pic 9v9       value 6.5.
         05 ws-4-half              pic 9v9       value 4.5.
         05 ws-12-half-prcent      pic 9v999     value 0.125.
         05 ws-8-half-prcent       pic 9v999     value 0.085.
         05 ws-6-half-prcent       pic 9v999     value 0.065.
         05 ws-4-half-prcent       pic 9v999     value 0.045.



      *****************************************************************
      *This is section for notifying us if we have reached end of line 
      *while reading the input file.
       01 ws-flags.
         05 ws-eof-flag            pic x         value "n".

       procedure division.

       000-main.
      *I have made paragraph for each function that has been carries 
      *over here

           perform 010-open-files.
           perform 020-write-headings.
           perform 030-read-input.

      *process each input record and read in the next record

           move spaces to ws-detail-line.
           write output-line from ws-detail-line.

           perform 100-process-file
             until ws-eof-flag equals "y".

           perform 110-total-calculation.
           perform 120-discount-total.
           perform 130-close-files.

           goback.

       100-process-file.
      *I am using paragraph inside my paragraph just to make my code 
      *look nice and clean and to suppress the amount of errors.

           move 0      to ws-extended-calc.
           move 0      to ws-discount-calc.
           move 0      to ws-net-price-calc.
           move 0      to ws-trans-per-calc.
           move 45     to ws-trans-charge-calc.
           add 1       to ws-total-items.

           perform 140-extended-price-calculation.
           perform 150-discount-calculation.
           perform 160-tranportation-charge-calculation.
           perform 170-count-discount-items.
           perform 180-net-price-calculation.
           perform 190-count-prices.
          
           perform 200-write-detail-line.
           perform 030-read-input.
           
      *open files
        010-open-files.
           open input input-file.
           open output output-file.

      *report heading
       020-write-headings.
           write output-line from ws-report-heading.
           write output-line from ws-title-1
           after advancing 2 lines.
           write output-line from ws-title-2.
      *   

      *initial read of input file
       030-read-input.

           read input-file
               at end
                   move "y" to ws-eof-flag.

      *in this paragraph i am summing up the price of extended price, 
      *net price and transportation charge
       110-total-calculation.
           move ws-price-count         to ws-total-extended.
           move ws-net-count           to ws-total-net.
           move ws-charge-count        to ws-total-trans-charge.

           write output-line from ws-summary-line
             after advancing 1 line.

      *In this paragraph i am calculating the number of items that got 
      *dicount on them and which did not and i am outputting percentage
      *of those who did not                                          
       120-discount-total.
           divide ws-discount-count by ws-total-items
             giving ws-interim rounded.

           multiply ws-interim by ws-100
             giving ws-percent-dicount.

           move ws-percent-sign-cnst to ws-discount-sign.

           write output-line from ws-overall-discount
             after advancing 3 lines.
             
      * Close files and end program
       130-close-files.
           close input-file.
           close output-file.

      *This section deals with the calculation of the extended price of
      *product.
       140-extended-price-calculation.
           multiply il-unit-price by il-quantity
             giving ws-extended-calc rounded.


      *Now, in this section we will be calculating discount for each 
      *product depending on product class and the quantity of an item.
       150-discount-calculation.
           if il-product-class = ws-a-class then
               if ws-extended-calc > ws-100 then
                   multiply ws-extended-calc by ws-5-percent giving
                     ws-discount-calc
               end-if
           else
               if il-product-class = ws-b-class then
                   if ws-extended-calc > ws-5 then
                       multiply ws-extended-calc by ws-5-percent giving
                         ws-discount-calc
                   end-if
               end-if
           end-if.

           if il-product-class = ws-f-class then
               if ws-extended-calc > ws-50 then
                   multiply ws-extended-calc by ws-5-percent giving
                     ws-discount-calc
               end-if
           end-if.


      *This section totally deals with the tranposrtaion percentage
      *and its charges.
       160-tranportation-charge-calculation.

           if il-product-class = ws-a-class then
               move ws-12-half to ws-trans-per-calc
               multiply ws-extended-calc by ws-12-half-prcent giving
                 ws-trans-charge-calc
           end-if.

           if il-product-class = ws-d-class then
               move ws-8-half to ws-trans-per-calc
               multiply ws-extended-calc by ws-8-half-prcent giving
                 ws-trans-charge-calc
           end-if.

           if il-product-class = ws-f-class then
               move ws-4-half to ws-trans-per-calc
               multiply ws-extended-calc by ws-4-half-prcent giving
                 ws-trans-charge-calc
           end-if.

           if (il-product-class = ws-b-class) or (il-product-class =
             ws-c-class) or (il-product-class = ws-z-class) or
             (il-product-class = ws-g-class) then
               if il-quantity <= ws-100 then
                   move ws-6-half to ws-trans-per-calc
                   multiply ws-extended-calc by ws-6-half-prcent giving
                     ws-trans-charge-calc
               end-if
           end-if.

      *This is the section in which we are counting the number of items 
      *    which got the dicount so that we can use it for calculation 
      *purposes later.
       170-count-discount-items.
           if ws-discount-calc = 0 then
               add 1 to ws-discount-count
           end-if.

      *This section just calculates the net price of the product 
      *subtracting discount price from extended price.
       180-net-price-calculation.
           subtract ws-discount-calc from ws-extended-calc giving
             ws-net-price-calc.

      *In this section we are adding to our extended price, net price 
      *and transportation charges so that can count its total later.
       190-count-prices.
           add ws-extended-calc        to ws-price-count.
           add ws-net-price-calc       to ws-net-count.
           add ws-trans-charge-calc    to ws-charge-count.

      *This section deals with the writting of detail line for our file.
       200-write-detail-line.
      *    
           move spaces to ws-detail-line.
           write output-line from ws-detail-line.

           move il-item-number             to ws-item-number.
           move il-product-class           to ws-product-class.
           move il-description             to ws-description.
           move il-quantity                to ws-quantity.
           move il-unit-price              to ws-unit-price.
           move ws-extended-calc           to ws-extended-price.
           move ws-discount-calc           to ws-discounted-price.
           move ws-net-price-calc          to ws-net-price.
           move ws-trans-per-calc          to ws-trans-percent.
           move ws-percent-sign-cnst       to ws-percent-sign.
           move ws-trans-charge-calc       to ws-trans-charge.

           write output-line from ws-detail-line.
      *   

       end program A2-ItemList.