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
                77 choix pic 9.
                77 choix2 pic 9.

        PROCEDURE DIVISION.

        PERFORM MENU_PRINCIPAL       

        STOP RUN.


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

        
     
        

        AJOUT_ENCLOS.
        OPEN i-o fenclos
        MOVE 0 TO idNonIdentique

        PERFORM WITH TEST AFTER UNTIL idNonIdentique = 1
            DISPLAY 'Donner un id à l enclos'
            ACCEPT wId
            MOVE wId to fe_numE
            READ fenclos
                INVALID KEY  MOVE 1 TO idNonIdentique
                NOT INVALID KEY DISPLAY "l'enclos existe deja"
            END-READ
        END-PERFORM
        
        DISPLAY 'Donner une capacité à l enclos'
        ACCEPT wCapEnclos
        DISPLAY 'Etat de l enclos'
        ACCEPT wEtatEnclos
        
 
         IF idNonIdentique = 1 THEN 
           
            MOVE wId TO fe_numE
            MOVE wCapEnclos TO fe_capacite
            MOVE wEtatEnclos TO fe_etat
            WRITE encl_tamp
            END-WRITE
            
        END-IF.
        CLOSE fenclos.


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
            

  

      

        MENU_PRINCIPAL.
        
        OPEN INPUT fenclos        
                IF fenclCR = 35 THEN
                        OPEN OUTPUT fenclos      
                END-IF
        CLOSE fenclos
        OPEN INPUT femployes        
                IF femplCR = 35 THEN
                        OPEN OUTPUT femployes      
                END-IF
        CLOSE femployes


        DISPLAY '1 = AJOUT ENCLOS'
        DISPLAY '2 = MODIFICATION ENCLOS'
        DISPLAY '3 = AFFICHAGE ENCLOS'
        DISPLAY '4 = AJOUT EMPLOYES'
        DISPLAY '5 = MODIF EMPLOYES '
        DISPLAY '6 = AFFICHAGE EMPLOYES'
        DISPLAY '7 = SUPPRIMER ENCLOS'
        DISPLAY '8 = SUPPRIMER EMPLOYES'
        ACCEPT choix
        EVALUATE  choix
	          WHEN "1" 
		        PERFORM AJOUT_ENCLOS
	          WHEN "2" 
		        PERFORM MODIF_ENCLOS
              WHEN "3"
                PERFORM AFFICHAGE_ENCLOS
              WHEN "4"
                PERFORM AJOUT_EMPLOYES
              WHEN "5"
                PERFORM MODIF_EMPLOYES
              WHEN "6"
                PERFORM AFFICHAGE_EMPLOYES
              WHEN "7"
                PERFORM SUPPRESSION_ENCLOS
              WHEN "8"
                PERFORM SUPPRESSION_EMPLOYES
        END-EVALUATE.


    








