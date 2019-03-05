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
                        02 fr_jour pic 9.
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

                77 idIdentique pic 9.
                77 wfin pic 9.

        PROCEDURE DIVISION.


        AJOUT_EMPLOYES.

        MOVE 0 TO idIdentique
        MOVE 0 TO wfin
        DISPLAY 'Donner un id à l employé'
        ACCEPT wId
        PERFORM WITH TEST AFTER UNTIL wfin = 1 or idIdentique = 1 
         
            
             READ femployes
                AT END  
                        MOVE 1 TO wfin
                NOT AT END
                        IF wId = fem_numEmp THEN
                             MOVE 1 TO idIdentique
                        END-IF                   
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

        IF idIdentique = 0 THEN 
            OPEN OUTPUT femployes
            MOVE wId TO fem_numEmpl
            MOVE wNomEmpl TO fem_nom
            MOVE wPrenomEmpl TO fem_prenom
            MOVE wDatenaissance TO fem_dateNaissance
            MOVE wDateEmbauche TO fem_dateEmbauche
            MOVE wtelephone TO fem_telephone
            MOVE wType TO fem_type
            WRITE empl_tamp
            END-WRITE
            CLOSE femployes
        END-IF.
            
    








