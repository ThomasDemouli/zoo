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
                        02 fs_numSoigneur pic 9(4).
                        02 fs_type pic A(30).
                        02 fs_numA pic 9(4).

        WORKING-STORAGE SECTION.
      * Variables communes
                77 fin PIC 9.
                77 fdf PIC 9.
                77 fenclCR pic 9(2).
                77 fanimCR pic 9(2).
                77 femplCR pic 9(2).
                77 frepaCR pic 9(2).
                77 fsoinCR pic 9(2).
                77 annee PIC 9(4).
                77 mois PIC 9(2).
                77 jour PIC 9(2).

      * Variables repas
                77 choix pic 9.
                77 bool pic 9.
                77 wFin pic 9.
                77 wNumR pic 9(9).
                77 wDesc pic A(999).
                77 wNumEmp pic 9(4).
                77 wNumA pic 9(3).
                77 wPrix pic 9(4).
                77 wHeure pic 9(4).
                77 wH pic 9(2).
                77 wM pic 9(2).
                77 wPhrase pic A(1040).

      * Variables soins
                77 numeroSoinValide PIC 9.
                77 numeroSoin PIC 9(4).
                77 descriptionSoin pic A(20).
                77 anneeSoin PIC 9(4).
                77 moisSoin PIC 9(2).
                77 jourSoin PIC 9(2).
                77 jourMax PIC 9(2).
                77 soigneurTrouve PIC 9(1).
                77 numeroSoigneur PIC 9(4).
                77 typeSoin PIC A(30).
                77 animalTrouve PIC 9(1).
                77 numeroAnimal PIC 9(4).
                77 typeSoinValide PIC 9(1).
                77 phraseSoin PIC A(999).

        PROCEDURE DIVISION.

        PERFORM APPELER_MENU
        STOP RUN.


      **************************************
      *                                    *
      *         FONCTIONS COMMUNES         *
      *                                    *
      **************************************
        MENU_PRINCIPAL.
        OPEN INPUT frepas        
                IF frepaCR = 35 THEN
                        OPEN OUTPUT frepas      
                END-IF
        CLOSE frepas
        OPEN INPUT femployes        
                IF femplCR = 35 THEN
                        OPEN OUTPUT femployes    
                END-IF
        CLOSE femployes
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
        OPEN INPUT fsoins        
                IF fsoinCR = 35 THEN
                        OPEN OUTPUT fsoins    
                END-IF
        CLOSE fsoins
        
        DISPLAY 'Que voulez vous faire ?'
        DISPLAY '1 = Gérer les animaux'
        DISPLAY '2 = Gérer les employés'
        DISPLAY '3 = Gérer les enclos'
        DISPLAY '4 = Gérer les soins'
        DISPLAY '5 = Gérer les repas'
        DISPLAY '0 = Quitter'
        ACCEPT choix
        EVALUATE  choix
      *           WHEN "1" 
      *	                PERFORM MENU_ANIMAUX
      *           WHEN "2" 
      *                 PERFORM MENU_EMPLOYES
      *           WHEN "3"
      *                 PERFORM MENU_ENCLOS
                  WHEN "4"
                        PERFORM MENU_SOINS
                  WHEN "5"
                        PERFORM MENU_REPAS

                  WHEN "0" 
                        MOVE 1 TO wfin
                  WHEN other 
                        DISPLAY "Commande non comprise " CHOIX
        END-EVALUATE.


      ******************************************************************
        DEMANDER_HEURE.
      * Demande de l'heure
        PERFORM WITH TEST AFTER UNTIL wH <= 23 AND wH >= 0
                DISPLAY 'L heure'
                ACCEPT wH
        END-PERFORM

      * Demande des minutes
        PERFORM WITH TEST AFTER UNTIL wM <= 59 AND wM >= 0
                DISPLAY 'Les minutes'
                ACCEPT wM
        END-PERFORM

      * Calcul de l'heure
        MULTIPLY wH BY 100 GIVING wHeure
        ADD wM TO wHeure GIVING wHeure.


      ******************************************************************
        APPELER_MENU.
        MOVE 0 to wfin
        PERFORM MENU_PRINCIPAL
        UNTIL wfin = 1.


      ******************************************************************
        DEMANDER_DATE.
        PERFORM WITH TEST AFTER UNTIL annee>1900 AND annee<2020
                DISPLAY "Quelle est l année ?"
                ACCEPT annee
        END-PERFORM

        PERFORM WITH TEST AFTER UNTIL mois>0 AND mois<13
                DISPLAY "Quel est le mois ?"
                ACCEPT mois
        END-PERFORM

        EVALUATE mois
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

        PERFORM WITH TEST AFTER UNTIL jour>0 AND jour<=jourMax
                DISPLAY "Quel est le jour ?"
                ACCEPT jour
        END-PERFORM.

      ***********************************
      *                                 *
      *         FONCTIONS REPAS         *
      *                                 *
      ***********************************
        MENU_REPAS.
        DISPLAY 'Que voulez-vous faire ?'
        DISPLAY '1 = AJOUT REPAS'
        DISPLAY '2 = SUPPRESSION REPAS'
        DISPLAY '3 = AFFICHER REPAS'
        DISPLAY '4 = MODIFIER REPAS'
        DISPLAY '0 = RETOUR'
        ACCEPT choix
        EVALUATE  choix
	          WHEN "1" 
		        PERFORM AJOUT_REPAS
                  WHEN "2" 
                        PERFORM SUPPRESSION_REPAS
                  WHEN "3"
                        PERFORM AFFICHAGE_REPAS
                  WHEN "4"
                        PERFORM MODIFIER_REPAS
                  WHEN "4"
                        PERFORM APPELER_MENU
        END-EVALUATE.

      ******************************************************************
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
        PERFORM DEMANDER_DATE
        MOVE annee TO fr_annee
        MOVE mois TO fr_mois
        MOVE jour TO fr_jour.

      * Demande de l'heure
        PERFORM DEMANDER_HEURE
        MOVE wHeure to fr_heure
        
      * Demande du numéro du soigneur
      * MOVE 0 TO bool
      * OPEN OUTPUT femployes
      * PERFORM WITH TEST AFTER UNTIL bool = 1
      *         DISPLAY 'Le numéro du soigneur'
      *         ACCEPT wNumEmp
      *         MOVE wNumEmp to fr_numSoigneur
      *         READ femployes
      *                 INVALID KEY MOVE 0 TO bool
      *                 NOT INVALID KEY MOVE 1 TO bool
      *         END-READ
      * END-PERFORM

        DISPLAY 'Le numéro du soigneur'
        ACCEPT wNumEmp
        MOVE wNumEmp to fr_numSoigneur

        CLOSE femployes

      * Demande du numéro de l'animal
      * MOVE 0 TO bool
      * OPEN OUTPUT fanimaux
      * PERFORM WITH TEST AFTER UNTIL bool = 1
      *         DISPLAY 'Le numéro de l'animal'
      *         ACCEPT wNumA
      *         MOVE wNumA to fr_numAnimal
      *         READ fanimaux
      *                 INVALID KEY MOVE 0 TO bool
      *                 NOT INVALID KEY MOVE 1 TO bool
      *         END-READ
      * END-PERFORM

        DISPLAY 'Le numéro de l animal'
        ACCEPT wNumA
        MOVE wNumA to fr_numAnimal

        CLOSE fanimaux

      * Demande du prix de repas
        DISPLAY 'Le prix du repas'
        ACCEPT wPrix
        MOVE wPrix to fr_prixRepas
        WRITE repa_tamp
        END-WRITE
        CLOSE frepas.

      ******************************************************************
        SUPPRESSION_REPAS.
      * Demande du numéro du repas
        DISPLAY 'Quel est le numéro du repas à supprimer ?'
        ACCEPT wNumR

      * Recherche du repas
        OPEN I-O frepas
        MOVE wNumR to fr_numR
        MOVE 0 TO bool
        READ frepas
                INVALID KEY MOVE 0 TO bool
                NOT INVALID KEY MOVE 1 TO bool
        END-READ

      * Suppression du repas
        IF bool = 0 THEN
                DISPLAY 'Ce repas n existe pas'
        END-IF
        IF bool = 1 THEN
                DELETE frepas RECORD
                DISPLAY 'repas supprimé'
        END-IF
        CLOSE frepas.

      ******************************************************************
        AFFICHAGE_REPAS.
      * Parcours séquentiel des repas
        OPEN INPUT frepas
        MOVE 0 TO wfin
        PERFORM WITH TEST AFTER UNTIL wfin = 1
                READ frepas NEXT
                AT END
                        MOVE 1 TO wfin
                NOT AT END
                        STRING fr_numR "|" fr_description "|" fr_jour
                        "|" fr_mois "|" fr_annee "|" fr_heure "|"  
                        fr_numSoigneur "|" fr_numAnimal "|" fr_prixRepas
                        INTO wPhrase
                        DISPLAY wPhrase
                END-READ
        END-PERFORM
        CLOSE frepas.

      ******************************************************************
        MODIFIER_REPAS.
      * Demande du numéro du repas
        DISPLAY 'Quel est le numéro du repas à modifier ?'
        ACCEPT wNumR

      * Recherche du repas
        OPEN I-O frepas
        MOVE wNumR to fr_numR
        MOVE 0 TO bool
        READ frepas
                INVALID KEY MOVE 0 TO bool
                NOT INVALID KEY MOVE 1 TO bool
        END-READ

      * Modification du repas
        IF bool = 0 THEN
                DISPLAY 'Ce repas n existe pas'
        END-IF
        IF bool = 1 THEN
                DISPLAY 'Que voulez-vous modifier ?'
                DISPLAY '1 = La description'
                DISPLAY '2 = La date'
                DISPLAY '3 = L heure'
                DISPLAY '4 = Le numero du soigneur'
                DISPLAY '5 = Le numero de l animal'
                DISPLAY '6 = Le prix du repas'
                ACCEPT choix
                EVALUATE  choix
      * Modification de la description
                      WHEN "1"
                                DISPLAY 'Nouvelle description :'
                                ACCEPT wDesc
                                MOVE wDesc to fr_description
                                DISPLAY 'Description modifiée'
      * Modification de la date
                      WHEN "2"
                                DISPLAY 'Nouvelle date :'
                                PERFORM DEMANDER_DATE
                                MOVE annee TO fr_annee
                                MOVE mois TO fr_mois
                                MOVE jour TO fr_jour
                                DISPLAY 'Date modifiée'

      * Modification de l'heure
                      WHEN "3"
                                DISPLAY 'Nouvelle heure :'
                                PERFORM DEMANDER_HEURE
                                MOVE wHeure to fr_heure
                                DISPLAY 'Heure modifiée'

      * Modification du numéro du soigneur
                      WHEN "4"
                                DISPLAY 'Nouveau numéro soigneur :'
                                ACCEPT wNumEmp
                                MOVE wNumEmp to fr_numSoigneur
                                DISPLAY 'Numéro soigneur modifié'

      * Modification du numéro de l'animal
                      WHEN "5"
                                DISPLAY 'Nouveau numéro animal :'
                                ACCEPT wNumA
                                MOVE wNumA to fr_numAnimal
                                DISPLAY 'Numéro animal modifié'

      * Modification du prix du repas
                      WHEN "6"
                                DISPLAY 'Nouveau prix :'
                                ACCEPT wPrix
                                MOVE wPrix to fr_prixRepas
                                DISPLAY 'prix du repas modifié'

                END-EVALUATE
        END-IF
        REWRITE repa_tamp
        END-REWRITE
        CLOSE frepas.



      ***********************************
      *                                 *
      *         FONCTIONS SOINS         *
      *                                 *
      ***********************************

        MENU_SOINS.
        DISPLAY "1 : Ajouter un soin"
        DISPLAY "2 : Lire tous les soins"
        DISPLAY "3 : Lire un soin"
        DISPLAY "4 : Modifier un soin"
        DISPLAY "5 : Supprimer un soin"
        DISPLAY "0 : Retour"
        ACCEPT choix
        EVALUATE choix
            when "1" PERFORM AJOUT_SOIN
            when "2" PERFORM AFFICHER_TOUS_LES_SOINS
            when "3" PERFORM AFFICHER_UN_SOIN
            when "4" PERFORM MODIFIER_UN_SOIN
            when "5" PERFORM SUPPRIMER_UN_SOIN
            when "0" PERFORM APPELER_MENU
            when other DISPLAY "Commande non comprise" CHOIX
        END-EVALUATE.

      ******************************************************************
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

        PERFORM AJOUT_DESCRIPTION_SOIN
        PERFORM AJOUT_DATE_SOIN 
        PERFORM AJOUT_SOIGNEUR_SOIN
        PERFORM AJOUT_TYPE_SOIN
        PERFORM AJOUT_ANIMAL_SOIN

        WRITE soin_tamp
        END-WRITE
        DISPLAY "Le soin a été créé !"
        CLOSE fsoins.

      ******************************************************************
        AFFICHER_TOUS_LES_SOINS.
        OPEN INPUT fsoins
        MOVE 0 TO fdf
        PERFORM WITH TEST AFTER UNTIL fdf=1
            READ fsoins NEXT
                AT END
                    DISPLAY "Fin du fichier"
                    MOVE 1 TO fdf
                NOT AT END
        STRING "SOIN n°" fs_numS ", Type : " fs_type ", Descriptif : "
        fs_descriptif ", Le " fs_jour "/" fs_mois "/" fs_annee
        ", Par le médecin n°" fs_numSoigneur ", Pour l'animal n°"
        fs_numA INTO phraseSoin
                    DISPLAY phraseSoin
            END-READ
        END-PERFORM
        CLOSE fsoins.

      ******************************************************************
        AFFICHER_UN_SOIN.
        OPEN INPUT fsoins
        DISPLAY "Quel est le numéro du soin ?"
        ACCEPT numeroSoin
        MOVE numeroSoin TO fs_numS
        READ fsoins
        INVALID KEY
            DISPLAY "Le soin n existe pas"
        NOT INVALID KEY
        STRING "SOIN n°" fs_numS ", Type : " fs_type ", Descriptif : "
        fs_descriptif ", Le " fs_jour "/" fs_mois "/" fs_annee
        ", Par le médecin n°" fs_numSoigneur ", Pour l'animal n°"
        fs_numA INTO phraseSoin
            DISPLAY phraseSoin
        END-READ
        CLOSE fsoins.

      ******************************************************************
        MODIFIER_UN_SOIN.
        OPEN I-O fsoins
        DISPLAY "Quel est le numéro du soin ?"
        ACCEPT numeroSoin
        MOVE numeroSoin TO fs_numS
        READ fsoins
        INVALID KEY
            DISPLAY "Le soin n existe pas"
        NOT INVALID KEY 
            DISPLAY "Quel attribut voulez-vous modifier ?"
                     DISPLAY "1 : La Description,"
                     DISPLAY "2 : La Date,"
                     DISPLAY "3 : Le Soigneur ayant fait le soin,"
                     DISPLAY "4 : Le Type du Soin,"
                     DISPLAY "5 : L animal qui a été soigné"
            ACCEPT choix
            EVALUATE choix
                when "1" 
                    PERFORM AJOUT_DESCRIPTION_SOIN
                when "2" 
                    PERFORM AJOUT_DATE_SOIN
                when "3" 
                    PERFORM AJOUT_SOIGNEUR_SOIN
                when "4" 
                    PERFORM AJOUT_TYPE_SOIN
                when "5" 
                    PERFORM AJOUT_ANIMAL_SOIN
                when other  
                DISPLAY "Commande non comprise" CHOIX
            END-EVALUATE
        END-READ
        REWRITE soin_tamp
        CLOSE fsoins.

      ******************************************************************
        SUPPRIMER_UN_SOIN.
        OPEN I-O fsoins
        DISPLAY "Quel est le numéro de l animal que vous voulez
        supprimer ?"
        ACCEPT numeroSoin
        MOVE numeroSoin TO fs_numS
        READ fsoins
            INVALID KEY DISPLAY "Le soin n existe pas"
            NOT INVALID KEY
                DELETE fsoins RECORD
                DISPLAY "Le soin a été supprimé !"
        END-READ
        CLOSE fsoins.

      ******************************************************************
        AJOUT_DESCRIPTION_SOIN.
            DISPLAY "Quel est sa description?"
            ACCEPT descriptionSoin
            MOVE descriptionSoin to fs_descriptif.

      ******************************************************************
        AJOUT_DATE_SOIN.
        PERFORM DEMANDER_DATE
        MOVE annee TO fs_annee
        MOVE mois TO fs_mois
        MOVE jour TO fs_jour.

      ******************************************************************
        AJOUT_SOIGNEUR_SOIN.
        OPEN INPUT femployes
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
        MOVE numeroSoigneur to fs_numSoigneur
        CLOSE femployes.

      ******************************************************************
        AJOUT_TYPE_SOIN.
        MOVE 0 TO typeSoinValide
        PERFORM WITH TEST AFTER UNTIL typeSoinValide = 1
                DISPLAY "Quel est le type du soin ?"
                DISPLAY "1 : Soin Maladie,"
                DISPLAY "2 : Soin Blessure,"
                DISPLAY "3 : Vaccin"
                ACCEPT typeSoin
                EVALUATE typeSoin
                WHEN "1"
                    MOVE "maladie" TO typeSoin
                    MOVE 1 TO typeSoinValide
                WHEN "2"
                    MOVE "blessure" TO typeSoin
                    MOVE 1 TO typeSoinValide
                WHEN "3"
                    MOVE "vaccin" TO typeSoin
                    MOVE 1 TO typeSoinValide
                END-EVALUATE
        END-PERFORM
        MOVE typeSoin TO fs_type.

      ******************************************************************
        AJOUT_ANIMAL_SOIN.
        OPEN INPUT fanimaux
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
        MOVE numeroAnimal TO fs_numA
        CLOSE fanimaux.




      *************************************
      *                                   *
      *         FONCTIONS ANIMAUX         *
      *                                   *
      *************************************





      **************************************
      *                                    *
      *         FONCTIONS EMPLOYES         *
      *                                    *
      **************************************





      ************************************
      *                                  *
      *         FONCTIONS ENCLOS         *
      *                                  *
      ************************************
