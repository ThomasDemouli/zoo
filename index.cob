                IDENTIFICATION DIVISION.

        PROGRAM-ID.parcZoologique.


                ENVIRONMENT DIVISION.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

        SELECT fenclos ASSIGN TO "enclos.dat"
        ORGANIZATION INDEXED
        ACCESS MODE Dynamic
        RECORD KEY fe_numE
        ALTERNATE RECORD KEY fe_etat with duplicates
        FILE STATUS is fenclCR.

        SELECT fanimaux ASSIGN TO "animaux.dat"
        ORGANIZATION INDEXED
        RECORD KEY fa_numA
        ALTERNATE RECORD KEY fa_numEnclos with duplicates
        ACCESS MODE DYNAMIC
        FILE STATUS is fanimCR.

        SELECT femployes ASSIGN TO "employes.dat"
        ORGANIZATION INDEXED
        RECORD KEY fem_numEmp
        ALTERNATE RECORD KEY fem_dateEmbauche with duplicates
        ACCESS MODE DYNAMIC
        FILE STATUS is femplCR.

        SELECT frepas ASSIGN TO "repas.dat"
        ORGANIZATION INDEXED
        RECORD KEY fr_numR
        ALTERNATE RECORD KEY fr_numAnimal with duplicates
        ACCESS MODE DYNAMIC
        FILE STATUS is frepaCR.

        SELECT fsoins ASSIGN TO "soins.dat"
        ORGANIZATION INDEXED
        RECORD KEY fs_numS
        ALTERNATE RECORD KEY fs_descriptif with duplicates
        ALTERNATE RECORD KEY fs_numA with duplicates
        ACCESS MODE DYNAMIC
        FILE STATUS is fsoinCR.


                DATA DIVISION.

        FILE SECTION.
        FD fenclos.
                01 encl_tamp.
                        02 fe_numE pic 9(3).
                        02 fe_capacite pic 9(2).
                        02 fe_etat pic A(25).

        FD fanimaux.
                01 anim_tamp.
                        02 fa_numA pic 9(3).
                        02 fa_surnom pic A(30).
                        02 fa_anneeNaissance pic 9(4).
                        02 fa_espece pic A(30).
                        02 fa_numEnclos pic 9(3).
                        02 fa_frequenceRepas pic 9(3).
                        02 fa_dernierRepas pic 9(9).
                        02 fa_dernierVaccin pic 9(9).

        FD femployes.
                01 empl_tamp.
                        02 fem_numEmp pic 9(4).
                        02 fem_nom pic A(30).
                        02 fem_prenom pic A(30).
                        02 fem_dateNaissance pic A(10).
                        02 fem_dateEmbauche pic A(10).
                        02 fem_telephone pic 9(10).
                        02 fem_type pic A(20).

        FD frepas.
                01 repa_tamp.
                        02 fr_numR pic 9(9).
                        02 fr_description pic A(999).
                        02 fr_jour pic 9(2).
                        02 fr_mois pic 9(2).
                        02 fr_annee pic 9(4).
                        02 fr_heure pic 9(4).
                        02 fr_numSoigneur pic 9(4).
                        02 fr_numAnimal pic 9(3).
                        02 fr_prixRepas pic 9(4).

        FD fsoins.
                01 soin_tamp.
                        02 fs_numS pic 9(4).
                        02 fs_descriptif pic A(20).
                        02 fs_date pic 9(8).
                        02 fs_numMedecin pic A(30).
                        02 fs_type pic A(30).
                        02 fs_numA pic 9(4).

        WORKING-STORAGE SECTION.
      * variable menu
                77 fin pic 9.
                77 choix pic 9.       


                77 fenclCR pic 9(2).
                77 fanimCR pic 9(2).
                77 femplCR pic 9(2).
                77 frepaCR pic 9(2).
                77 fsoinCR pic 9(2).

      * variable ajout_animal / modif_animal
                77 numeroAValide pic 9.
                77 enclosNonExistant pic 9.
                77 fdfA pic 9.
                77 numA pic 9(3).
                77 surnomA pic A(30).
                77 aNaissA pic 9(4).
                77 especeA pic A(30).
                77 numEnclosA pic 9(3).
                77 frequenceRepasA pic 9(3).
                77 dernierRepasA pic 9(9).
                77 dernierVaccinA pic 9(9).

      * variable capacite_enclos
                77 CE pic 9.
                77 enclosComplet pic 9.
                77 cptCE pic 9(3).
                77 capaciteEnclos pic 9(2).
                77 fdf pic 9.



        PROCEDURE DIVISION.
        OPEN INPUT fanimaux
        IF fanimCR = 35 THEN
                OPEN OUTPUT fanimaux
        END-IF
        CLOSE fanimaux

        OPEN INPUT fenclos
        IF fenclCR = 35 THEN
                OPEN OUTPUT fenclos
        END-IF
        CLOSE fenclos

        MOVE 0 to fin.
        PERFORM MENU_ANIMAL
          UNTIL fin = 1.
        STOP RUN.

        MENU_ANIMAL.

        DISPLAY "       0: Quitter,
                 1 : Ajouter un animal,
                 2 : Capaciter enclos".
        ACCEPT choix.
        EVALUATE choix
        when "1" PERFORM AJOUT_ANIMAL
        when "2" PERFORM CAPACITE_ENCLOS
        when "0" move 1 to fin
        when other DISPLAY "Commande non comprise" CHOIX
        END-EVALUATE.

        AJOUT_ANIMAL.
        OPEN I-O fanimaux
        MOVE 0 TO numeroAValide
        PERFORM WITH TEST AFTER UNTIL numeroAValide = 1
             DISPLAY "Quel est l'identifiant de l'animal(entier)?"
             ACCEPT numA
             MOVE numA to fa_numA
             READ fanimaux
                     INVALID KEY MOVE 1 TO numeroAValide
                     NOT INVALID KEY MOVE 0 TO numeroAValide
             END-READ
        END-PERFORM
        MOVE numA TO fa_numA        

        DISPLAY "Quel est son surnom?"
        ACCEPT surnomA
        MOVE surnomA to fa_surnom

        PERFORM WITH TEST AFTER UNTIL aNaissA>1800 AND aNaissA<2019
             DISPLAY "Quelle est son année de naissance?"
             ACCEPT aNaissA
        END-PERFORM

        MOVE aNaissA to fa_anneeNaissance 
        DISPLAY "Quel est son espece?"
        ACCEPT especeA
        MOVE especeA to fa_espece

        MOVE 0 TO enclosNonExistant
        MOVE 0 TO fdfA
        OPEN INPUT fenclos
        PERFORM WITH TEST AFTER UNTIL enclosNonExistant = 1 OR fdfA=1
        OR enclosComplet = 0
            DISPLAY "Dans quel enclos est-il?"
            ACCEPT numEnclosA
            READ fenclos
              AT END
                    MOVE 1 TO fdfA
              NOT AT END
                IF fa_numEnclos = numEnclosA THEN 
                    MOVE 1 TO enclosNonExistant
                END-IF
           END-READ     
                MOVE numEnclosA TO CE
                PERFORM CAPACITE_ENCLOS
                IF enclosComplet = 1 THEN
                    DISPLAY "Cet enclos est complet"
                END-IF
        END-PERFORM
        CLOSE fenclos
        DISPLAY "Quel est sa fréquence de repas?"
        ACCEPT frequenceRepasA


        MOVE numEnclosA to fa_numEnclos
        MOVE frequenceRepasA to fa_frequenceRepas
            
        WRITE anim_tamp
        END-WRITE
        DISPLAY "Animal ajouté !"
        CLOSE fanimaux.
            
    
       CAPACITE_ENCLOS.
       OPEN INPUT fenclos
       MOVE 0 TO enclosComplet
       MOVE CE to fe_numE
       READ fenclos
            INVALID KEY DISPLAY "Cet enclos n'existe pas" 
            NOT INVALID KEY MOVE fe_capacite TO capaciteEnclos
       END-READ
       CLOSE fenclos
       OPEN INPUT fanimaux
       MOVE CE TO fa_numEnclos
       MOVE 0 TO fdf
       START fanimaux, KEY IS = fa_numEnclos
            INVALID KEY MOVE 0 TO enclosComplet
            NOT INVALID KEY PERFORM WITH TEST AFTER UNTIL fdf=1
                  READ fanimaux NEXT 
                  AT END MOVE 1 TO fdf
                  NOT AT END  ADD 1 TO cptCE
                  END-READ 
            END-PERFORM                     
       END-START 
       CLOSE fanimaux  
       IF capaciteEnclos - cptCE <=0 THEN 
                MOVE 1 TO enclosComplet
       ELSE
                MOVE 0 TO enclosComplet
       END-IF.
      
       MODIF_ANIMAL.
       OPEN I-O fanimaux
       MOVE 0 TO numeroAValide
       PERFORM WITH TEST AFTER UNTIL numeroAValide = 1
       DISPLAY "Quel animal voulez vous modifié ? Entrez son identifiant"
             ACCEPT numA
             MOVE numA to fa_numA
             READ fanimaux
                     INVALID KEY "Animal non existant"
                     NOT INVALID KEY MOVE 1 TO numeroAValide
             END-READ
       END-PERFORM
       MOVE numA to fa_numA
      * lecture direct + demande des nouveaux attributs 


