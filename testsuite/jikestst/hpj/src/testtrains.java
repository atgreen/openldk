// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class testtrains {
  public static void main(String argv[]) {
      Train train1, train2, train3;
      Mfg pullman, ge, emd;
      int result = 0;

      pullman = new Mfg();
//      pullman.name = "PULLMAN";
      pullman.employees = 100;
      
      ge = new Mfg();
//      ge.name = "GE";
      ge.employees = 10000;

      emd = new Mfg();
//      emd.name = "EMD";
      emd.employees = 2000;

      train1 = new Train();
      train1.serial = 1;
      train1.passengercars = 5;
      train1.freightcars = 6;
      train1.manufacturer = pullman;
      train1.machinepowered = true;
      train1.numberpassengers = 5;

      train2 = new Train();
      train2.serial = 2;
      train2.passengercars = 8;
      train2.freightcars = 4;
      train2.manufacturer = ge;
      train2.machinepowered = true;
      train2.numberpassengers = train2.passengercars * 1;

      train3 = new Train();
      train3.serial = 3;
      train3.passengercars = 3;
      train3.freightcars = 2;
      train3.manufacturer = emd;
      train3.machinepowered = true;
      train3.numberpassengers = 3;

      int totalpcars = train1.passengercars + train2.passengercars + 
                      train3.passengercars;
      int totalfcars = train1.freightcars + train2.freightcars + 
                      train3.freightcars;
      int totalpass = train1.numberpassengers + train2.numberpassengers +
                      train3.numberpassengers;
      
      if (totalpass != 16) {
         System.out.println(totalpass);
         System.exit(totalpass);
       }

      if (totalpcars != 16 || totalfcars != 12 || totalpass != 16) {
        result = totalpass;
        System.out.println(result);
        System.exit(result);
        result = result + 10;
      }
      
      if (train3.manufacturer.employees < train1.manufacturer.employees)
        result = result + 100;

      if (train3.manufacturer != train2.manufacturer)
        result = result + 57;

      System.out.println(result);
      System.exit(result);
    }
}
