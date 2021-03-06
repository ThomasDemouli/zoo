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
                        02 fa_dernierVaccin pic 9(4).

        FD femployes.
                01 empl_tamp.
                        02 fem_numEmp pic 9(4).
                        02 fem_nom pic A(30).
                        02 fem_prenom pic A(30).
                        02 fem_dateNaissance pic A(10).
                        02 fem_annee_embauche pic 9(4).
                        02 fem_mois_embauche pic 9(2).
                        02 fem_jour_embauche pic 9(2).
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

      * variables ajout_animal / modif_animal / suppr_animal
                77 numeroAValide pic 9.
                77 enclosNonExistant pic 9.
                77 numA pic 9(3).
                77 surnomA pic A(30).
                77 aNaissA pic 9(4).
                77 especeA pic A(30).
                77 numEnclosA pic 9(3).
                77 frequenceRepasA pic 9(3).
                77 dernierRepasA pic 9(9).
                77 dernierVaccinA pic 9(9).
                77 descriptionA pic A(999).
                77 choixModif pic 9.
        	    77 nbJoursDiff PIC 9(7).
        	    77 dateRepas PIC 9(8).
        		77 phraseBesoin PIC A(100).
        		77 phrasePrix PIC A(200).
                77 fdz PIC 9(1).
                77 cptrRepas PIC 9(9).
                77 sommePrix PIC 9(9).
                77 prixMoyen PIC 9(3).
                77 res PIC 9(3).

      * variables capacite_enclos
                77 CE pic 9.
                77 enclosComplet pic 9.
                77 cptCE pic 9(3).
                77 capaciteEnclos pic 9(2).

      * variables enclos / employes
                77 wId pic 9(3).
                77 wNomEmpl pic A(25).
                77 wPrenomEmpl pic A(25).
                77 wDatenaissance pic A(10).
                77 wtelephone pic 9(10).
                77 wtype pic A(20).
                77 wCapEnclos pic 9(2).
                77 wEtatEnclos pic A(20).
                77 idIdentique pic 9.
                77 idNonIdentique pic 9.
                77 wdatecourante pic 9(4).
                77 wjourEmbauche pic 9(2).
                77 wmoisEmbauche pic 9(2).
                77 wanneeEmbauche pic 9(4).

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
                  WHEN "1"
       	                PERFORM MENU_ANIMAUX
                  WHEN "2"
                        PERFORM MENU_EMPLOYES
                  WHEN "3"
                        PERFORM MENU_ENCLOS
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
        DISPLAY '1 = Ajouter un repas'
        DISPLAY '2 = Supprimer un repas'
        DISPLAY '3 = Afficher un repas'
        DISPLAY '4 = Modifier un repas'
        DISPLAY '0 = Retour'
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
                        NOT INVALID KEY DISPLAY 'Numéro deja existant'
                END-READ
        END-PERFORM

      * Demande de la description du repas
        DISPLAY 'La description du repas'
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
        MOVE 0 TO bool
        OPEN INPUT femployes
        PERFORM WITH TEST AFTER UNTIL bool = 1
                DISPLAY 'Le numéro du soigneur'
                ACCEPT wNumEmp
                MOVE wNumEmp to fem_numEmp
                READ femployes
                        INVALID KEY DISPLAY 'Soigneur inexistant'
                        NOT INVALID KEY MOVE 1 TO bool
                END-READ
        END-PERFORM
        MOVE wNumEmp to fr_numSoigneur
        CLOSE femployes

      * Demande du numéro de l'animal
        MOVE 0 TO bool
        OPEN INPUT fanimaux
        PERFORM WITH TEST AFTER UNTIL bool = 1
                DISPLAY 'Le numéro de l animal'
                ACCEPT wNumA
                MOVE wNumA to fa_numA
                READ fanimaux
                        INVALID KEY DISPLAY 'Animal inexistant'
                        NOT INVALID KEY MOVE 1 TO bool
                END-READ
        END-PERFORM
        MOVE wNumA to fr_numAnimal
        CLOSE fanimaux

      * Demande du prix de repas
        DISPLAY 'Le prix du repas'
        ACCEPT wPrix
        MOVE wPrix to fr_prixRepas
        WRITE repa_tamp
        END-WRITE
        CLOSE frepas

      * Modification du dernier repas de l'animal
        OPEN I-O fanimaux
        MOVE wNumA to fa_numA
        MOVE wNumR to fa_dernierRepas
        REWRITE anim_tamp
        END-REWRITE
        CLOSE fanimaux.

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
        MOVE 0 TO fin
        PERFORM WITH TEST AFTER UNTIL fin = 1
                READ frepas NEXT
                AT END
                        MOVE 1 TO fin
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
        CLOSE fsoins

      * Modification du dernier vaccin de l'animal
        IF typeSoin = 'vaccin' THEN
                OPEN I-O fanimaux
                MOVE numeroAnimal to fa_numA
                MOVE numeroSoin to fa_dernierVaccin
                REWRITE anim_tamp
                END-REWRITE
                CLOSE fanimaux
        END-IF.

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
               READ femployes
               INVALID KEY DISPLAY "Le soigneur n existe pas"
               NOT INVALID KEY MOVE 1 TO soigneurTrouve
               END-READ
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
               READ fanimaux
               INVALID KEY DISPLAY "L animal n existe pas"
               NOT INVALID KEY MOVE 1 TO animalTrouve
               END-READ
        END-PERFORM
        MOVE numeroAnimal TO fs_numA
        CLOSE fanimaux.




      *************************************
      *                                   *
      *         FONCTIONS ANIMAUX         *
      *                                   *
      *************************************

        MENU_ANIMAUX.
        DISPLAY 'Que voulez vous faire ?'
        DISPLAY '1 : Ajouter un animal'
        DISPLAY '2 : Supprimer un animal'
        DISPLAY '3 : Afficher un animal'
        DISPLAY '4 : Afficher tous les animaux'
        DISPLAY '5 : Modifier un animal'
        DISPLAY '6 : Liste des soins administrés à un animal'
        DISPLAY '7 : Liste des repas d un animal'
        DISPLAY '8 : Voir quel animal a besoin d etre nourri'
        DISPLAY '9 : Définir le prix moyen d un repas'
        DISPLAY '0 : Retour'
        ACCEPT choix
        EVALUATE choix
                WHEN "1" PERFORM AJOUT_ANIMAL
                WHEN "2" PERFORM SUPPRIMER_ANIMAL
                WHEN "3" PERFORM AFFICHER_ANIMAL
                WHEN "4" PERFORM AFFICHER_TOUS_LES_ANIMAUX
                WHEN "5" PERFORM MODIFIER_ANIMAL
                WHEN "6" PERFORM SOINSADMINISTREANIMAL
                WHEN "7" PERFORM LISTE_REPAS_ANIMAL
                WHEN "8" PERFORM ANIMAUX_BESOIN_REPAS
                WHEN "9" PERFORM PRIX_MOYEN_REPAS
                WHEN "0" PERFORM APPELER_MENU
                WHEN other DISPLAY "Commande non comprise" CHOIX
        END-EVALUATE.


      ******************************************************************
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

        DISPLAY "Quel est son surnom?"
        ACCEPT surnomA


        PERFORM WITH TEST AFTER UNTIL aNaissA>1800 AND aNaissA<2019
             DISPLAY "Quelle est son année de naissance?"
             ACCEPT aNaissA
        END-PERFORM


        DISPLAY "Quel est son espece?"
        ACCEPT especeA


        MOVE 1 TO enclosComplet
        MOVE 0 TO enclosNonExistant
        OPEN INPUT fenclos
        PERFORM WITH TEST AFTER UNTIL enclosNonExistant = 1
        AND enclosComplet = 0
            DISPLAY "Dans quel enclos est-il?"
            ACCEPT numEnclosA
            MOVE numEnclosA to fe_numE
            READ fenclos
                INVALID KEY DISPLAY "Enclos non existant"
                NOT INVALID KEY
                MOVE 1 TO enclosNonExistant
                MOVE numEnclosA TO CE
                PERFORM CAPACITE_ENCLOS
                IF enclosComplet = 1 THEN
                    DISPLAY "Cet enclos est complet"
                END-IF
            END-READ
        END-PERFORM
        CLOSE fenclos
        MOVE numEnclosA to fa_numEnclos
        DISPLAY numEnclosA
        DISPLAY "Quel est sa fréquence de repas (en nombre de jours)?"
        ACCEPT frequenceRepasA
        MOVE frequenceRepasA to fa_frequenceRepas
        MOVE especeA to fa_espece
        MOVE surnomA to fa_surnom
        MOVE numA TO fa_numA
        MOVE aNaissA to fa_anneeNaissance
        MOVE 0 to fa_dernierRepas
        MOVE 0 to fa_dernierVaccin
        DISPLAY fa_numA
        WRITE anim_tamp
                INVALID KEY DISPLAY "ERREUR : animal non ajouté"
                NOT INVALID KEY DISPLAY "Animal ajouté !"
        END-WRITE
        CLOSE fanimaux.


      ******************************************************************
      * procedure determinant si un enclos est complet ou pas
       CAPACITE_ENCLOS.
       OPEN INPUT fenclos
       MOVE CE to fe_numE
       READ fenclos
            INVALID KEY MOVE 0 to enclosNonExistant
            NOT INVALID KEY MOVE fe_capacite TO capaciteEnclos
       END-READ
       CLOSE fenclos
      *on verifie la place restante
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
       IF capaciteEnclos - cptCE <=0 THEN
                MOVE 1 TO enclosComplet

           ELSE
                MOVE 0 TO enclosComplet
       END-IF.


      ******************************************************************
       SUPPRIMER_ANIMAL.
       OPEN I-O fanimaux
       DISPLAY "Entrez le numéro de l'animal à supprimer"
       ACCEPT numA
       MOVE numA TO fa_numA
       READ fanimaux
           INVALID KEY DISPLAY "L'animal n'existe pas"
           NOT INVALID KEY
                DELETE fanimaux RECORD
                DISPLAY "L'animal a été supprimé"
       END-READ
       CLOSE fanimaux.


      ******************************************************************
       AFFICHER_ANIMAL.
       OPEN I-O fanimaux
       DISPLAY "Entrez le numéro de l'animal à afficher"
       ACCEPT numA
       MOVE numA TO fa_numA
       READ fanimaux
           INVALID KEY DISPLAY "L'animal n'existe pas"
           NOT INVALID KEY
                  STRING "ANIMAL n° " fa_numA ", Surnom :"
                    fa_surnom ", Naissance :" fa_anneeNaissance
                    ", Espece :" fa_espece ", Enclos :" fa_numEnclos
                    ", Frequence repas : " fa_frequenceRepas
                    ", Dernier vaccin : " fa_dernierVaccin
                    ", Dernier repas : " fa_dernierRepas
                    into descriptionA
                    DISPLAY descriptionA
       END-READ
       CLOSE fanimaux.


      ******************************************************************
        AFFICHER_TOUS_LES_ANIMAUX.
        OPEN INPUT fanimaux
        MOVE 0 to fdf
        PERFORM WITH TEST AFTER UNTIL fdf =1
            READ fanimaux NEXT
                AT END
                    DISPLAY "fin de fichier"
                    MOVE 1 TO fdf
                NOT AT END
                    STRING "ANIMAL n° " fa_numA ", Surnom :"
                    fa_surnom ", Naissance :" fa_anneeNaissance
                    ", Espece :" fa_espece ", Enclos :" fa_numEnclos
                    ", Frequence repas : " fa_frequenceRepas
                    ", Dernier vaccin : " fa_dernierVaccin
                    ", Dernier repas : " fa_dernierRepas
                    into descriptionA
                    DISPLAY descriptionA
            END-READ
        END-PERFORM
        CLOSE fanimaux.


      ******************************************************************
        MODIFIER_ANIMAL.
      * Demande du numéro de l'animal
        DISPLAY "Quel est le numéro de l'animal à modifier ?"
        ACCEPT numA

      * Recherche de l'animal
        OPEN I-O fanimaux
        MOVE numA to fa_numA
        MOVE 0 TO numeroAValide
        READ fanimaux
                INVALID KEY MOVE 0 TO numeroAValide
                NOT INVALID KEY MOVE 1 TO numeroAValide
        END-READ

      * Modification de l'animal
        IF numeroAValide = 0 THEN
                DISPLAY "Cet animal n'existe pas"
        END-IF
        IF numeroAValide = 1 THEN
                DISPLAY 'Que voulez-vous modifier ?'
                DISPLAY '1 = Le surnom'
                DISPLAY '2 = Annee de naissance '
                DISPLAY '3 = Espece '
                DISPLAY '4 = Numero enclos'
                DISPLAY '5 = Frequence repas'
                ACCEPT choixModif
                EVALUATE  choixModif
      * Modification du surnom
                      WHEN "1"
                                DISPLAY 'Nouveau surnom:'
                                ACCEPT surnomA
                                MOVE surnomA to fa_surnom
                                DISPLAY 'surnom modifiée'
      * Modification de l'annee de naissance
                      WHEN "2"
                                DISPLAY 'Nouvelle annee de naissance :'
                                PERFORM WITH TEST
                                AFTER UNTIL aNaissA>1800 AND aNaissA<2019
                                ACCEPT aNaissA
                                END-PERFORM
                                MOVE aNaissA to fa_anneeNaissance
                                DISPLAY 'Annee de naissance modifiée'

      * Modification de l'espece
                      WHEN "3"
                                DISPLAY 'Nouvelle espece :'
                                MOVE especeA to fa_espece
                                DISPLAY 'Espece modifiée'

      * Modification du numéro de l'enclos
                      WHEN "4"
        PERFORM WITH TEST AFTER UNTIL enclosNonExistant = 1
        OR enclosComplet = 0
                                DISPLAY 'Nouveau numéro enclos :'
                                ACCEPT numEnclosA
                                MOVE numEnclosA TO CE
                                PERFORM CAPACITE_ENCLOS
        END-PERFORM
                                MOVE numEnclosA to fa_numEnclos
                                DISPLAY 'Numéro enclos modifié'

      * Modification de la frequence
                      WHEN "5"
                               DISPLAY 'Nouvelle fréquence repas :'
                               ACCEPT frequenceRepasA
                               MOVE frequenceRepasA to fa_frequenceRepas
                               DISPLAY 'Frquence repas modifié'
                END-EVALUATE
        END-IF
        REWRITE anim_tamp
        END-REWRITE
        CLOSE fanimaux.


      ******************************************************************
        SOINSADMINISTREANIMAL.
        OPEN INPUT fsoins
        DISPLAY 'Quel est le numero de l animal'
        ACCEPT wNumA
        MOVE wNumA TO fs_numA
        MOVE 0 TO fdf
        START fsoins, KEY IS = fs_numA
             INVALID KEY DISPLAY 'L animal n a recu aucun soin'
             NOT INVALID KEY PERFORM WITH TEST AFTER UNTIL fdf=1
                   READ fsoins NEXT
                   AT END MOVE 1 TO fdf
                   NOT AT END DISPLAY fs_numS
                   END-READ
            END-PERFORM
        END-START
        CLOSE fsoins.


      ******************************************************************
        LISTE_REPAS_ANIMAL.
        OPEN INPUT fanimaux
        MOVE 0 TO animalTrouve
        PERFORM WITH TEST AFTER UNTIL animalTrouve = 1
                DISPLAY "Quel est le numéro de l animal ?"
                ACCEPT numeroAnimal
                MOVE numeroAnimal TO fa_numA
               READ fanimaux
               INVALID KEY DISPLAY "L animal n existe pas"
               NOT INVALID KEY
                    MOVE 1 TO animalTrouve
                    MOVE fa_surnom TO surnomA
               END-READ
        END-PERFORM
        CLOSE fanimaux
        DISPLAY "L'animal ", surnomA, " de numéro ", numeroAnimal","
        DISPLAY "a eu pour repas :"
        OPEN INPUT frepas
        MOVE numeroAnimal TO fr_numAnimal
            START frepas, KEY IS = fr_numAnimal
            INVALID KEY DISPLAY 'Aucun repas'
            NOT INVALID KEY
            PERFORM WITH TEST AFTER UNTIL fdf=1
                READ frepas NEXT
                    AT END MOVE 1 TO fdf
                    NOT AT END
                    DISPLAY "Numéro du repas :", fr_numR
                    DISPLAY "Description du repas :", fr_description
                    DISPLAY "Date :", fr_jour SPACE fr_mois
                                      SPACE fr_annee, " à :"
                                      fr_heure
                END-READ
            END-PERFORM
        END-START
        CLOSE frepas.

      **************************************
      *                                    *
      *         FONCTIONS EMPLOYES         *
      *                                    *
      **************************************

        MENU_EMPLOYES.
        DISPLAY 'Que voulez vous faire ?'
        DISPLAY '1 : Ajouter un employe'
        DISPLAY '2 : Modifier un employe'
        DISPLAY '3 : Supprimer un employe'
        DISPLAY '4 : Afficher un employe'
        DISPLAY '5 : Affiche les nouveaux employes'
        DISPLAY '0 : Retour'
        ACCEPT choix
        EVALUATE choix
                WHEN "1" PERFORM AJOUT_EMPLOYES
                WHEN "2" PERFORM MODIF_EMPLOYES
                WHEN "3" PERFORM SUPPRESSION_EMPLOYES
                WHEN "4" PERFORM AFFICHAGE_EMPLOYES
                WHEN "5" PERFORM NOUVEL_EMPLOYE
                WHEN "0" PERFORM APPELER_MENU
                WHEN other DISPLAY "Commande non comprise" CHOIX
        END-EVALUATE.


      ******************************************************************
        AJOUT_EMPLOYES.
        OPEN i-o femployes
        MOVE 0 TO idNonIdentique

        PERFORM WITH TEST AFTER UNTIL idNonIdentique = 1
            DISPLAY 'Donner un id à l employé'
            ACCEPT wId
            MOVE wId to fem_numEmp
            READ femployes
                INVALID KEY MOVE 1 TO idNonIdentique
                NOT INVALID KEY DISPLAY "l'employé existe deja"
            END-READ

        END-PERFORM

        DISPLAY 'Nom de l employe'
        ACCEPT wNomEmpl
        DISPLAY 'Prenom de l employe'
        ACCEPT wPrenomEmpl
        DISPLAY 'Date de naissance de l employé'
        ACCEPT wDatenaissance
        DISPLAY 'Date d embauche de l employé'
        PERFORM DEMANDER_DATE
        DISPLAY 'Numéro de téléphone de l employé'
        ACCEPT wtelephone
        DISPLAY 'Type de l employé'
        ACCEPT wType

        IF idNonIdentique = 1 THEN
            MOVE wId TO fem_numEmp
            MOVE wNomEmpl TO fem_nom
            MOVE wPrenomEmpl TO fem_prenom
            MOVE wDatenaissance TO fem_dateNaissance
            MOVE annee TO fem_annee_embauche
            MOVE mois TO fem_mois_embauche
            MOVE jour TO fem_jour_embauche
            MOVE wtelephone TO fem_telephone
            MOVE wType TO fem_type
            WRITE empl_tamp
            END-WRITE
        END-IF
        CLOSE femployes.


      ******************************************************************
        MODIF_EMPLOYES.
        OPEN i-o femployes
        MOVE 0 TO idIdentique
            PERFORM WITH TEST AFTER UNTIL idIdentique = 1
                DISPLAY 'Donner l id de l employé à modifier '
                ACCEPT wId
                MOVE wId to fem_numEmp
                READ femployes
                INVALID KEY  DISPLAY "l'employé n'existe pas"
                NOT INVALID KEY MOVE 1 TO idIdentique
                END-READ
            END-PERFORM

          IF idIdentique = 1 THEN
            DISPLAY 'Que voulez vous modifier ?'
            DISPLAY '1 : Son nom ? ?'
            DISPLAY '2 : Son numéro de téléphone?'
            DISPLAY '3 : Son type ?'

            ACCEPT choix
            EVALUATE  choix
            WHEN "1"
	            DISPLAY 'Nouveau nom à l employé'
                ACCEPT wNomEmpl
                MOVE wNomEmpl to fem_nom
            WHEN "2"
	            DISPLAY 'Nouveau numéro de tél de l employé'
               ACCEPT wtelephone
               MOVE wtelephone TO fem_telephone
            WHEN "3"
	            DISPLAY 'Nouveau type à l employé'
               ACCEPT wtype
               MOVE wtype TO fem_type
            END-EVALUATE

        END-IF
        REWRITE empl_tamp
        END-REWRITE
        CLOSE femployes.


      ******************************************************************
        SUPPRESSION_EMPLOYES.
        OPEN i-o femployes
        MOVE 0 TO idIdentique
            PERFORM WITH TEST AFTER UNTIL idIdentique = 1
                DISPLAY 'Donner l id de l employes à supprimer '
                ACCEPT wId
                MOVE wId to fem_numEmp
                READ femployes
                INVALID KEY  DISPLAY "l'employé n'existe pas"
                NOT INVALID KEY MOVE 1 TO idIdentique
                END-READ
            END-PERFORM

        if idIdentique = 1 then
            delete femployes record
        end-if
        close femployes.


      ******************************************************************
        AFFICHAGE_EMPLOYES.
        OPEN INPUT femployes
        MOVE 0 TO fin
        PERFORM WITH TEST AFTER UNTIL fin = 1
                READ femployes next
                AT END
                        MOVE 1 TO fin
                NOT AT END
                        DISPLAY fem_numEmp
                        DISPLAY fem_nom
                        DISPLAY fem_prenom
                        DISPLAY fem_dateNaissance
                        DISPLAY fem_annee_embauche
                        DISPLAY fem_mois_embauche
                        DISPLAY fem_jour_embauche
                        DISPLAY fem_telephone
                        DISPLAY fem_type
                END-READ
        END-PERFORM
        CLOSE femployes.


      ******************************************************************
        NOUVEL_EMPLOYE.
        OPEN INPUT femployes
        MOVE FUNCTION CURRENT-DATE(1:4) TO wdatecourante
        PERFORM WITH TEST AFTER UNTIL wfin = 1

                READ femployes next
                AT END
                        MOVE 1 TO wfin
                NOT AT END
                       if  fem_annee_embauche = wdatecourante
                            DISPLAY fem_nom
                            DISPLAY fem_prenom
                            DISPLAY fem_type
                            display "a ete embauché cette annee"
                END-READ

        END-PERFORM
        close femployes.





      ************************************
      *                                  *
      *         FONCTIONS ENCLOS         *
      *                                  *
      ************************************

        MENU_ENCLOS.
        DISPLAY 'Que voulez vous faire ?'
        DISPLAY '1 : Ajouter un enclos'
        DISPLAY '2 : Modifier un enclos'
        DISPLAY '3 : Afficher un enclos'
        DISPLAY '4 : Capaciter enclos'
        DISPLAY '5 : Supprimer un enclos'
        DISPLAY '6 : verifier etat'
        DISPLAY '7 : verifier capacite enclos'
        DISPLAY '0 : Retour'
        ACCEPT choix
        EVALUATE choix
                when "1" PERFORM AJOUT_ENCLOS
                when "2" PERFORM MODIF_ENCLOS
                when "3" PERFORM AFFICHAGE_ENCLOS
                when "4" PERFORM CAPACITE_ENCLOS
                when "5" PERFORM SUPPRESSION_ENCLOS
                when "6" PERFORM AFFICHER_ENCLOS_ETAT
                when "7" PERFORM ENCLOS_COMPLET
                WHEN "0" PERFORM APPELER_MENU
                WHEN other DISPLAY "Commande non comprise" CHOIX
        END-EVALUATE.


      ******************************************************************
        AJOUT_ENCLOS.
        OPEN i-o fenclos
        MOVE 0 TO idIdentique

        PERFORM WITH TEST AFTER UNTIL idIdentique = 1
            DISPLAY 'Donner un id à l enclos'
            ACCEPT wId
            MOVE wId to fe_numE
            READ fenclos
            INVALID KEY  MOVE 1 TO idIdentique
            NOT INVALID KEY DISPLAY "l'enclos existe deja"

            END-READ
        END-PERFORM
        CLOSE fenclos
        DISPLAY 'Donner une capacité à l enclos'
        ACCEPT wCapEnclos
        DISPLAY 'Etat de l enclos'
        ACCEPT wEtatEnclos
        CLOSE fenclos

         IF idIdentique = 1 THEN
            OPEN i-o fenclos
            MOVE wId TO fe_numE
            MOVE wCapEnclos TO fe_capacite
            MOVE wEtatEnclos TO fe_etat
            WRITE encl_tamp
            END-WRITE
            CLOSE fenclos
        END-IF.


      ******************************************************************
        MODIF_ENCLOS.
        OPEN i-o fenclos
        MOVE 0 TO idIdentique
            PERFORM WITH TEST AFTER UNTIL idIdentique = 1
                DISPLAY 'Donner l id de l enclos à modifier '
                ACCEPT wId
                MOVE wId to fe_numE
                READ fenclos
                INVALID KEY  DISPLAY "l'enclos n'existe pas"
                NOT INVALID KEY MOVE 1 TO idIdentique
                END-READ
            END-PERFORM

          IF idIdentique = 1 THEN
            DISPLAY 'Que voulez vous modifier ?'
            DISPLAY '1 : Sa capacité ? ?'
            DISPLAY '2 : Son etat'


            ACCEPT choix
            EVALUATE  choix
            WHEN "1"
	            DISPLAY 'Nouvelle capacité de l enclos'
                ACCEPT wCapEnclos
                MOVE wCapEnclos to fe_capacite
            WHEN "2"
	            DISPLAY 'Nouvel etat de l enclos'
               ACCEPT wEtatEnclos
               MOVE wEtatEnclos TO fe_etat

            END-EVALUATE

        END-IF
        REWRITE encl_tamp
        END-REWRITE
        CLOSE fenclos.


      ******************************************************************
        SUPPRESSION_ENCLOS.
        OPEN i-o fenclos
        MOVE 0 TO idIdentique
            PERFORM WITH TEST AFTER UNTIL idIdentique = 1
                DISPLAY 'Donner l id de l enclos à supprimer '
                ACCEPT wId
                MOVE wId to fe_numE
                READ fenclos
                INVALID KEY  DISPLAY "l'enclos n'existe pas"
                NOT INVALID KEY MOVE 1 TO idIdentique
                     MOVE fe_capacite TO capaciteEnclos
            END-READ
         END-PERFORM

      * on verifie si il est vide
        OPEN INPUT fanimaux
        MOVE wId TO fa_numEnclos
        MOVE 0 TO fdf
        START fanimaux, KEY IS = fa_numEnclos
            INVALID KEY DISPLAY ' '
            NOT INVALID KEY
            PERFORM WITH TEST AFTER UNTIL fdf=1
                READ fanimaux NEXT
                    AT END MOVE 1 TO fdf
                    NOT AT END  ADD 1 TO cptCE
                END-READ
            END-PERFORM
        END-START
        CLOSE fanimaux


        if idIdentique = 1 and cptCE = 0 then
            delete fenclos record
            DISPLAY "Enclos supprimé"
        else
            DISPLAY " Erreur : Enclos non vide "
        end-if
        close fenclos.


      ******************************************************************
        AFFICHAGE_ENCLOS.
        OPEN INPUT fenclos
        MOVE 0 TO fin
        PERFORM WITH TEST AFTER UNTIL fin = 1
                READ fenclos next
                AT END
                        MOVE 1 TO fin
                NOT AT END
                        DISPLAY fe_numE
                        DISPLAY fe_capacite
                        DISPLAY fe_etat
                END-READ
        END-PERFORM
        CLOSE fenclos.


      ******************************************************************
        AFFICHER_ENCLOS_ETAT.
        OPEN INPUT fenclos
        display "Vous voulez voir les enclos dans quel etat ?"
        accept wEtatEnclos
        MOVE 0 TO fin
        PERFORM WITH TEST AFTER UNTIL fin = 1
                READ fenclos next
                AT END
                        MOVE 1 TO fin
                NOT AT END
                       if wEtatEnclos = fe_etat
                          display "L enclos numéro "fe_numE" d une"
                          display "capacité de" fe_capacite
                          display "est" fe_etat

                END-READ
        END-PERFORM
        close fenclos.

      ******************************************************************
        ENCLOS_COMPLET.
        MOVE 0 TO enclosNonExistant
        OPEN INPUT fenclos
        PERFORM WITH TEST AFTER UNTIL enclosNonExistant = 1
            DISPLAY "Rentrez le numero de l'enclos dont vous voulez "
            DISPLAY "connaitre sa capacite"
            ACCEPT wId
            MOVE wId to fe_numE
            READ fenclos
                INVALID KEY DISPLAY "Enclos non existant"
                NOT INVALID KEY
                    MOVE 1 TO enclosNonExistant
                    MOVE fe_capacite TO capaciteEnclos
            END-READ
         END-PERFORM
        CLOSE fenclos
      * on verifie la place restante
        OPEN INPUT fanimaux
        MOVE wId TO fa_numEnclos
        MOVE 0 TO fdf
        START fanimaux, KEY IS = fa_numEnclos
            INVALID KEY DISPLAY ' '
            NOT INVALID KEY
            PERFORM WITH TEST AFTER UNTIL fdf=1
                READ fanimaux NEXT
                    AT END MOVE 1 TO fdf
                    NOT AT END  ADD 1 TO cptCE
                END-READ
            END-PERFORM
        END-START
        CLOSE fanimaux
        IF capaciteEnclos - cptCE <=0 THEN
                DISPLAY "L'enclos est complet"
           ELSE
            COMPUTE cptCE = capaciteEnclos - cptCE
            DISPLAY "Il reste ", cptCE
                    " places dans l'enclos"
        END-IF.

      *****************************************************************
        	ANIMAUX_BESOIN_REPAS.
        	OPEN INPUT fanimaux
            MOVE 0 TO fdf
            PERFORM WITH TEST AFTER UNTIL fdf=1
                READ fanimaux NEXT
                AT END
                    DISPLAY "Traitement terminé"
                    MOVE 1 TO fdf
                NOT AT END
        			OPEN INPUT frepas
        			MOVE fa_dernierRepas TO fr_numR
        			READ frepas
                  INVALID KEY
                      DISPLAY "Le repas n existe plus"
                  NOT INVALID KEY
         COMPUTE dateRepas = fr_annee * 10000 + fr_mois * 100 + fr_jour
          COMPUTE nbJoursDiff = FUNCTION
          INTEGER-OF-DATE(FUNCTION CURRENT-DATE(1:8)) - FUNCTION
          INTEGER-OF-DATE(dateRepas)
                      IF nbJoursDiff >= fa_frequenceRepas THEN
            STRING " Cet animal a besoin d etre nourri : " fa_surnom
            " n°" fa_numA INTO phraseBesoin
                			DISPLAY phraseBesoin
            			END-IF
                  END-READ
        			CLOSE frepas
                END-READ
            END-PERFORM
            CLOSE fanimaux.

      ****************************************************************
          PRIX_MOYEN_REPAS.
          OPEN INPUT fanimaux
          DISPLAY "De quel animal voulez-vous étudier les prix de son
          repas ?"
          ACCEPT numeroAnimal
          MOVE numeroAnimal TO fa_numA
          READ fanimaux
          INVALID KEY
              DISPLAY "L animal n existe pas"
          NOT INVALID KEY
              OPEN INPUT frepas
                  MOVE 0 TO fdz
                  MOVE fa_numA TO fr_numAnimal
                  START frepas, KEY IS = fr_numAnimal
          		INVALID KEY
                      DISPLAY "Cet animal ne possède aucun repas"
                  NOT INVALID KEY
                      PERFORM WITH TEST AFTER UNTIL fdz=1
                          READ frepas NEXT
                          AT END
                              MOVE 1 TO fdz
                          NOT AT END
                              ADD fr_prixRepas TO sommePrix
                              ADD 1 TO cptrRepas
                          END-READ
                  END-PERFORM
                  COMPUTE prixMoyen = sommePrix / cptrRepas
                  STRING "Le prix moyen de l animal " fa_surnom " n°"
                  fa_numA " est de " prixMoyen "€" INTO phrasePrix
                  DISPLAY phrasePrix
              END-START
              CLOSE frepas
          END-READ
          CLOSE fanimaux.
