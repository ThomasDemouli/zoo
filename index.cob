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
                        02 fs_jour pic 9(2).
                        02 fs_mois pic 9(2).
                        02 fs_annee pic 9(2).
                        02 fs_numMedecin pic 9(4).
                        02 fs_type pic A(30).
                        02 fs_numA pic 9(4).

        WORKING-STORAGE SECTION.
                77 fin PIC 9.
                77 choix PIC 9.
                77 fdf PIC 9.
                77 fenclCR pic 9(2).
                77 fanimCR pic 9(2).
                77 femplCR pic 9(2).
                77 frepaCR pic 9(2).
                77 fsoinCR pic 9(2).

                77 numeroSoinValide PIC 9.
                77 numeroSoin PIC 9(4).
                77 descriptionS pic A(20).
                77 anneeSoin PIC 9(4).
                77 moisSoin PIC 9(2).
                77 jourSoin PIC 9(2).
                77 jourMax PIC 9(2).
                77 soigneurTrouve PIC 9(1).
                77 numeroSoigneur PIC 9(4).
                77 typeSoin PIC A(30).
                77 animalTrouve PIC 9(1).
                77 numeroAnimal PIC 9(4).

        PROCEDURE DIVISION.

        OPEN INPUT fsoins
        IF fsoinCR = 35 THEN
                OPEN OUTPUT fsoins
        END-IF
        CLOSE fsoins

        MOVE 0 to fin.
        PERFORM PROGRAMME_PRINCIPAL
          UNTIL fin = 1.
        STOP RUN.

        PROGRAMME_PRINCIPAL.

        DISPLAY "       0: Quitter,
                 1 : Ajouter un soin,
                 2 : Lire tous les soins".
        ACCEPT choix.
        EVALUATE choix
        when "1" PERFORM AJOUT_SOIN
        when "2" PERFORM READ_SOIN
        when "0" move 1 to fin
        when other DISPLAY "Commande non comprise" CHOIX
        END-EVALUATE.

        AJOUT_SOIN.
        OPEN I-O fsoins
        MOVE 0 TO numeroSoinValide
        PERFORM WITH TEST AFTER UNTIL numeroSoinValide = 1
                DISPLAY "Quel sera le numéro de soin"
                ACCEPT numeroSoin
                MOVE numeroSoin TO fs_numS
                READ fsoins
                        INVALID KEY MOVE 1 TO numeroSoinValide
                        NOT INVALID KEY DISPLAY "Le soin existe déjà"
                END-READ
        END-PERFORM

        DISPLAY "Quel est sa description?"
        ACCEPT descriptionS
        MOVE descriptionS to fs_descriptif

        PERFORM WITH TEST AFTER UNTIL anneeSoin>1900 AND anneeSoin<2019
                DISPLAY "Quelle est  l année du soin ?"
                ACCEPT anneeSoin
        END-PERFORM
        MOVE anneeSoin to fs_annee

        PERFORM WITH TEST AFTER UNTIL moisSoin>0 AND moisSoin<13
                DISPLAY "Quel est  le mois du soin ?"
                ACCEPT moisSoin
        END-PERFORM
        MOVE moisSoin to fs_mois

        EVALUATE moisSoin
        WHEN 1 MOVE 31 TO jourMax
        WHEN 2 MOVE 28 TO jourMax
        WHEN 3 MOVE 31 TO jourMax
        WHEN 4 MOVE 30 TO jourMax
        WHEN 5 MOVE 31 TO jourMax
        WHEN 6 MOVE 30 TO jourMax
        WHEN 7 MOVE 31 TO jourMax
        WHEN 8 MOVE 31 TO jourMax
        WHEN 9 MOVE 30 TO jourMax
        WHEN 10 MOVE 31 TO jourMax
        WHEN 11 MOVE 30 TO jourMax
        WHEN 12 MOVE 31 TO jourMax
        END-EVALUATE

        PERFORM WITH TEST AFTER UNTIL jourSoin>0 AND jourSoin<jourMax
                DISPLAY "Quel est  le jour du soin ?"
                ACCEPT jourSoin
        END-PERFORM
        MOVE jourSoin to fs_jour

        MOVE 0 TO soigneurTrouve
        PERFORM WITH TEST AFTER UNTIL soigneurTrouve = 1
                DISPLAY "Quel est le numéro du soigneur ?"
                ACCEPT numeroSoigneur
                MOVE numeroSoigneur TO fem_numEmp
      *         READ femployes
      *         INVALID KEY DISPLAY "Le soigneur n existe pas"
      *         NOT INVALID KEY MOVE 1 TO soigneurTrouve
      *         END-READ
                MOVE 1 TO soigneurTrouve
        END-PERFORM
        MOVE numeroSoigneur to fs_numMedecin

        PERFORM WITH TEST AFTER UNTIL typeSoin>0 AND typeSoin<4
                DISPLAY "Quel est le type du soin ?"
                DISPLAY "1 : Soin Maladie"
                DISPLAY "2 : Soin Blessure"
                DISPLAY "3 : Vaccin"
                ACCEPT typeSoin
        END-PERFORM
        MOVE jourSoin to fs_jour

        EVALUATE moisSoin
        WHEN 1 MOVE "maladie" TO typeSoin
        WHEN 2 MOVE "blessure" TO typeSoin
        WHEN 3 MOVE "vaccin" TO typeSoin
        END-EVALUATE
        MOVE typeSoin TO fs_type

        MOVE 0 TO animalTrouve
        PERFORM WITH TEST AFTER UNTIL animalTrouve = 1
                DISPLAY "Quel est le numéro de l animal ?"
                ACCEPT numeroAnimal
                MOVE numeroAnimal TO fa_numA
      *         READ fanimaux
      *         INVALID KEY DISPLAY "L animal n existe pas"
      *         NOT INVALID KEY MOVE 1 TO animalTrouve
      *         END-READ
                MOVE 1 TO animalTrouve
        END-PERFORM
        MOVE numeroAnimal to fs_numA

        WRITE soin_tamp
        END-WRITE
        DISPLAY "Le soin a été créé !"
        CLOSE fsoins.



        READ_SOIN.
        OPEN INPUT fsoins
        MOVE 0 TO fdf
        PERFORM WITH TEST AFTER UNTIL fdf=1
            READ fsoins
                 AT END
                       MOVE 1 TO fdf
                       DISPLAY "Fin du fichier"
                 NOT AT END
                       DISPLAY "SOIN n°"
                       DISPLAY fs_numS
                       DISPLAY fs_type
                       DISPLAY fs_descriptif
                       DISPLAY "Le "
                       DISPLAY fs_jour
                       DISPLAY fs_mois
                       DISPLAY fs_annee
                       DISPLAY "Par le médecin n°"
                       DISPLAY fs_numMedecin
                       DISPLAY "Animal n°"
                       DISPLAY fs_numA
            END-READ
        END-PERFORM
        CLOSE fsoins.


        ATTENTION, LA METHODE 2 NE MARCHE PAS !!!!!!!!!!!
