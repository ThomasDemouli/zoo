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
                77 fenclCR pic 9(2).
                77 fanimCR pic 9(2).
                77 femplCR pic 9(2).
                77 frepaCR pic 9(2).
                77 fsoinCR pic 9(2).

                77 choix pic 9.
                77 bool pic 9.

                77 wNumR pic 9(9).
                77 wDesc pic A(999).
                77 wNumEmp pic 9(4).
                77 wNumA pic 9(3).
                77 wPrix pic 9(4).

                77 wHeure pic 9(4).
                77 wH pic 9(2).
                77 wM pic 9(2).

        PROCEDURE DIVISION.

        PERFORM MENU_REPAS
        STOP RUN.

        MENU_REPAS.
        OPEN INPUT frepas        
                IF frepaCR = 35 THEN
                        OPEN OUTPUT frepas      
                END-IF
        CLOSE frepas

        DISPLAY '1 = AJOUT REPAS'
        DISPLAY '2 = SUPPRESSION REPAS'
        DISPLAY '3 = AFFICHER REPAS'
        DISPLAY '4 = MODIFIER REPAS'
        ACCEPT choix
        EVALUATE  choix
	          WHEN "1" 
		        PERFORM AJOUT_REPAS
      *           WHEN "2" 
      *	                PERFORM SUPPRESSION_REPAS
      *           WHEN "3"
      *                 PERFORM AFFICHER_REPAS
      *           WHEN "4"
      *                 PERFORM MODIFIER_REPAS
        END-EVALUATE.

        AJOUT_REPAS.
        OPEN I-O frepas

      * Demande du numéro du repas
        MOVE 0 TO bool
        PERFORM WITH TEST AFTER UNTIL bool = 1
                DISPLAY 'Le numéro de repas'
                ACCEPT wNumR
                MOVE wNumR to fr_numR
                READ frepas
                        INVALID KEY MOVE 1 TO bool
                        NOT INVALID KEY MOVE 0 TO bool
                END-READ
        END-PERFORM

      * Demande de la description du repas
        DISPLAY 'La descritpion du repas'
        ACCEPT wDesc
        MOVE wDesc to fr_description

      * Demande de la date

      * Demande de l'heure
        PERFORM DEMANDER_HEURE
        MOVE wHeure to fr_heure
        
      * Demande du numéro du soigneur
        MOVE 0 TO bool
        OPEN OUTPUT femployes
        PERFORM WITH TEST AFTER UNTIL bool = 1
                DISPLAY 'Le numéro du soigneur'
                ACCEPT wNumEmp
                MOVE wNumEmp to fr_numSoigneur
                READ femployes
                        INVALID KEY MOVE 0 TO bool
                        NOT INVALID KEY MOVE 1 TO bool
                END-READ
        END-PERFORM
        CLOSE femployes

      * Demande du numéro de l'animal
        MOVE 0 TO bool
        OPEN OUTPUT fanimaux
        PERFORM WITH TEST AFTER UNTIL bool = 1
                DISPLAY 'Le numéro de l animal'
                ACCEPT wNumA
                MOVE wNumA to fr_numAnimal
                READ fanimaux
                        INVALID KEY MOVE 0 TO bool
                        NOT INVALID KEY MOVE 1 TO bool
                END-READ
        END-PERFORM
        CLOSE fanimaux

      * Demande du prix de repas
        DISPLAY 'Le prix du repas'
        ACCEPT wPrix

        CLOSE frepas.


      * SUPPRESSION_REPAS.

      * AFFICHER_REPAS.

      * MODIFIER_REPAS.

        DEMANDER_HEURE.
      * Demande de l'heure
        PERFORM WITH TEST AFTER UNTIL wH <= 23 AND wH > 0
                DISPLAY 'L heure'
                ACCEPT wH
        END-PERFORM

      * Demande des minutes
        PERFORM WITH TEST AFTER UNTIL wM <= 59 AND wM > 0
                DISPLAY 'Les minutes'
                ACCEPT wM
        END-PERFORM

      * Calcul de l'heure
        MULTIPLY wH BY 100 GIVING wHeure
        ADD wM TO wHeure GIVING wHeure.
