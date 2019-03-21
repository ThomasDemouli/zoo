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
                        02 fem_jour_embauche pic 9(2).
                        02 fem_mois_embauche pic 9(2).
                        02 fem_annee_embauche pic 9(4).
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

      * variable ajout_animal / modif_animal / suppr_animal
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
                77 descriptionA pic A(999).

      * variable capacite_enclos
                77 CE pic 9.
                77 enclosComplet pic 9.
                77 cptCE pic 9(3).
                77 capaciteEnclos pic 9(2).
                77 fdf pic 9.

      * variable enclos / employes
                77 wId pic 9(3).
                77 wNomEmpl pic A(25).
                77 wPrenomEmpl pic A(25).
                77 wDatenaissance pic A(10).
                77 wDateEmbauche pic A(10).       
                77 wtelephone pic 9(10).
                77 wtype pic A(20).
                77 wCapEnclos pic 9(2).
                77 wEtatEnclos pic A(20).

                77 idIdentique pic 9.
                77 idNonIdentique pic 9.
                77 wfin pic 9.



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

        DISPLAY "Que voulez vous voir ?
                0: Quitter,
                1: les animaux,
                2: les enclos ,
                3: les employes
                    "

           
        accept choix
        evaluate choix
        when "1" 
            display "Que voulez vous faire ?
                    1 : Ajouter un animal,
                    2 : Supprimer un animal,
                    3 : Afficher un animal,
                    4 : Afficher tous les animaux,
                    "
                    accept choix
                    evaluate choix
                    when "1" PERFORM AJOUT_ANIMAL
                    when "2" PERFORM SUPPRIMER_ANIMAL
                    when "3" PERFORM AFFICHER_ANIMAL
                    when "4" PERFORM AFFICHER_TOUS_LES_ANIMAUX
                    END-EVALUATE
        when "2" 
            display "Que voulez vous faire ?
                    1 : Ajouter un enclos,
                    2 : Modifier un enclos,
                    3 : Afficher un enclos,
                    4 : Capaciter enclos,
                    5 : Supprimer un enclos,
                    6 : verifier etat
                " 
                accept choix
                evaluate choix
                when "1" PERFORM AJOUT_ENCLOS
                when "2" PERFORM MODIF_ENCLOS
                when "3" PERFORM AFFICHAGE_ENCLOS
                when "4" PERFORM CAPACITE_ENCLOS
                when "5" PERFORM SUPPRESSION_ENCLOS
                when "6" PERFORM AFFICHER_ENCLOS_ETAT
                END-EVALUATE
        when "3"
            display "Que voulez vous faire ?
                 1 : Ajouter un employe,
                 2 : Modifier un employe,
                 3 : Supprimer un employe,
                 4 : Afficher un employe
                "     
                accept choix 
                evaluate choix
                when "1" PERFORM AJOUT_EMPLOYES
                when "2" PERFORM MODIF_EMPLOYES
                when "3" PERFORM SUPPRESSION_EMPLOYES
                when "4" PERFORM AFFICHER_EMPLOYES
                END-EVALUATE

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
        MOVE numEnclosA to fa_numEnclos
        CLOSE fenclos
        DISPLAY "Quel est sa fréquence de repas (en nombre de jours)?"
        ACCEPT frequenceRepasA
        MOVE frequenceRepasA to fa_frequenceRepas

              
        MOVE 0 to fa_dernierRepas
        MOVE 0 to fa_dernierVaccin  

        WRITE anim_tamp

        END-WRITE
        DISPLAY "Animal ajouté !"
        CLOSE fanimaux.
            
      * procedure determinant si un enclos est complet ou pas
       CAPACITE_ENCLOS.
       OPEN INPUT fenclos
       MOVE 0 TO enclosComplet
       MOVE CE to fe_numE
       READ fenclos
            INVALID KEY DISPLAY "Cet enclos n'existe pas" 
            NOT INVALID KEY MOVE fe_capacite TO capaciteEnclos
       END-READ
      *Ajouter un if enclos existe (pour enlever l'affichage enclos complet lorsque l'enclos n'existe pas)
       CLOSE fenclos
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
      
       MODIF_ANIMAL.
      *   OPEN I-O fanimaux
      *  MOVE 0 TO numeroAValide
      * PERFORM WITH TEST AFTER UNTIL numeroAValide = 1
      * DISPLAY "Quel animal voulez vous modifié ? Entrez son identifiant"
      *        ACCEPT numA
      *       MOVE numA to fa_numA
      *        READ fanimaux
      *                INVALID KEY DISPLAY "Animal non existant"
      *                NOT INVALID KEY MOVE 1 TO numeroAValide
      *        END-READ
      *  END-PERFORM
      *  MOVE numA to fa_numA.
      * lecture direct + demande des nouveaux attributs

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

       AFFICHER_ANIMAL.
       OPEN I-O fanimaux
       DISPLAY "Entrez le numéro de l'animal à afficher"
       ACCEPT numA
       MOVE numA TO fa_numA
       READ fanimaux
           INVALID KEY DISPLAY "L'animal n'existe pas"
           NOT INVALID KEY 
                STRING "test" INTO descriptionA
                DISPLAY descriptionA
       END-READ
       CLOSE fanimaux.
       
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

      * --------------------GESTION ENCLOS-------------------------------------------
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
                END-READ
            END-PERFORM

        if idIdentique = 1 then
            delete fenclos record 
        end-if
        close fenclos.

       

        AFFICHAGE_ENCLOS.
        OPEN INPUT fenclos
        MOVE 0 TO wfin
        PERFORM WITH TEST AFTER UNTIL wfin = 1
                READ fenclos next
                AT END  
                        MOVE 1 TO wfin
                NOT AT END
                        DISPLAY fe_numE
                        DISPLAY fe_capacite
                        DISPLAY fe_etat
                END-READ
        END-PERFORM
        CLOSE fenclos.

        AFFICHER_EMPLOYES.
        OPEN INPUT fenclos
        MOVE 0 TO wfin
        PERFORM WITH TEST AFTER UNTIL wfin = 1
                READ fenclos next
                AT END  
                        MOVE 1 TO wfin
                NOT AT END
                        DISPLAY fe_numE
                        DISPLAY fe_capacite
                        DISPLAY fe_etat
                END-READ
        END-PERFORM
        CLOSE fenclos.

      *--------------------EMPLOYE ------------------------------
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
        ACCEPT wDateEmbauche
        DISPLAY 'Numéro de l employé'
        ACCEPT wtelephone
        DISPLAY 'Type de l employé'
        ACCEPT wType
 
        IF idNonIdentique = 1 THEN  
            MOVE wId TO fem_numEmp
            MOVE wNomEmpl TO fem_nom
            MOVE wPrenomEmpl TO fem_prenom
            MOVE wDatenaissance TO fem_dateNaissance
            MOVE wDateEmbauche TO fem_dateEmbauche
            MOVE wtelephone TO fem_telephone
            MOVE wType TO fem_type
            WRITE empl_tamp
            END-WRITE
        END-IF
        CLOSE femployes.
      
            
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

        AFFICHAGE_EMPLOYES.
        OPEN INPUT femployes
        MOVE 0 TO wfin
        PERFORM WITH TEST AFTER UNTIL wfin = 1
                READ femployes next
                AT END  
                        MOVE 1 TO wfin
                NOT AT END
                        DISPLAY fem_numEmp
                        DISPLAY fem_nom
                        DISPLAY fem_prenom
                        DISPLAY fem_dateNaissance  
                        DISPLAY fem_dateEmbauche  
                        DISPLAY fem_telephone
                        DISPLAY fem_type
                END-READ
        END-PERFORM
        CLOSE femployes.


        AFFICHER_ENCLOS_ETAT.
        OPEN INPUT fenclos
        display "Vous voulez voir les enclos dans quel etat ?"
        accept wEtatEnclos
        PERFORM WITH TEST AFTER UNTIL wfin = 1
                READ fenclos next
                AT END  
                        MOVE 1 TO wfin
                NOT AT END
                       if wEtatEnclos = fe_etat
                          display "L enclos numéro "fe_numE" d une"
                          display "capacité de" fe_capacite 
                          display "est" fe_etat
                                        
                END-READ
        END-PERFORM
        close fenclos.

       

        NOUVEL_EMPLOYE
         OPEN INPUT femployes
        
        PERFORM WITH TEST AFTER UNTIL wfin = 1
                MOVE FUNCTION CURRENT-DATE(1:8) TO wdatejour
                READ fenclos next
                AT END  
                        MOVE 1 TO wfin
                NOT AT END
                       if  fem_annee_embauche = fe_etat
                          display "L enclos numéro "fe_numE" d une"
                          display "capacité de" fe_capacite 
                          display "est" fe_etat
                                        
                END-READ
        END-PERFORM
        close fenclos.
        
        
            




