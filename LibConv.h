//LibConv.h
// 
// LibConv.dll
// par Eric SIBERT
// http://eric.sibert.fr
//
// Utilisation de la DLL LibConv.dll en Visual C++
// Francis Tourneur : f.tourneur@free.fr
// 26/02/2006
//
// La classe C++ CConversion est un "wrapper" pour les fonctions implémentées dans
// LibConv. La classe est incluse dans l'espace de nom GeoConversion.
// Avant d'utiliser une implémentation de la classe inclure :
//    using namespace GeoConversion
// 
// Les fonctions membres de la classe CConversion ont les mêmes paramètres
// que les fonction de l'API contenue dans LibConv.dll.
//
#pragma once

typedef int  (CALLBACK * LPFNDLLNBPROJ)();
typedef int  (CALLBACK * LPFNDLLNBMULT)();
typedef int  (CALLBACK * LPFNDLLNBELLIPSOIDE)();
typedef int  (CALLBACK * LPFNDLLNBDATUM)();
typedef int  (CALLBACK * LPFNDLLNBPROJACTIVE)();
typedef int  (CALLBACK * LPFNDLLNBMULTACTIVE)();
typedef int  (CALLBACK * LPFNDLLNOMPROJ)(int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLNOMMULT)(int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLNBPARAMPROJ)(int);
typedef int  (CALLBACK * LPFNDLLNOMPARAMPROJ)(int, int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLNBPARAMMULT)(int);
typedef int  (CALLBACK * LPFNDLLNOMPARAMMULT)(int, int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLLITFICHIER)(LPCTSTR, int);
typedef int  (CALLBACK * LPFNDLLECRITFICHIER)(LPCTSTR, int);
typedef int  (CALLBACK * LPFNDLLAJOUTERCAS)(LPCTSTR, int);
typedef int  (CALLBACK * LPFNDLLNOMELLIPSOIDE)(int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLNOMDATUM)(int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLNOMPROJACTIVE)(int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLNOMMULTACTIVE)(int, LPTSTR, int);
typedef int  (CALLBACK * LPFNDLLDATUMTOPROJ)(LPCTSTR, int, double, double, int, LPTSTR, int, double *, double *, int *);
typedef int  (CALLBACK * LPFNDLLDATUMTODATUM)(LPCTSTR, int, double, double, int,LPTSTR, int, double *, double *);
typedef int  (CALLBACK * LPFNDLLPROJTODATUM)(LPCTSTR, int, double, double, int, LPTSTR, int, double *, double *);
typedef int  (CALLBACK * LPFNDLLPROJTOPROJ)(LPCTSTR, int, double, double, int, LPTSTR, int, double *, double *, int *);
typedef BOOL (CALLBACK * LPFNDLLCONVERSION)(LPTSTR, int, LPTSTR, int, LPTSTR , int , LPTSTR YSource, int SizeYSource, int, int *, LPTSTR, int, LPTSTR, int, int);
typedef BOOL (CALLBACK * LPFNDLLCONVERGENCE)(LPTSTR, int, LPTSTR, int, LPTSTR, int, LPTSTR, int, int, LPTSTR, int, int);
typedef BOOL (CALLBACK * LPFNDLLALTERATION)(LPTSTR, int, LPTSTR, int, LPTSTR, int, LPTSTR, int, int, double *);
typedef BOOL (CALLBACK * LPFNDLLPROJGETA)(LPTSTR, int, double *);
typedef BOOL (CALLBACK * LPFNDLLPROJGETINVF)(LPTSTR, int, double *);
typedef BOOL (CALLBACK * LPFNDLLPROJGETDATUM)(LPTSTR, int, LPTSTR, int);

namespace GeoConversion {
class CConversion
{
public:
	CConversion() : m_hLibConv(NULL)
	{
		m_hLibConv = LoadLibrary(_T("LibConv.dll"));
		if (m_hLibConv != NULL) {
			m_lpfnDllNbProj        = (LPFNDLLNBPROJ)GetProcAddress(m_hLibConv, _T("Nb_Projection"));
			m_lpfnDllNomProj       = (LPFNDLLNOMPROJ)GetProcAddress(m_hLibConv, _T("Nom_Projection")); 
			m_lpfnDllNbMult        = (LPFNDLLNBMULT)GetProcAddress(m_hLibConv, _T("Nb_Multiple"));
			m_lpfnDllNomMult       = (LPFNDLLNOMMULT)GetProcAddress(m_hLibConv, _T("Nom_Multiple")); 
			m_lpfnDllNbParamProj   = (LPFNDLLNBPARAMPROJ)GetProcAddress(m_hLibConv, _T("Nb_Param_Proj"));
			m_lpfnDllNomParamProj  = (LPFNDLLNOMPARAMPROJ)GetProcAddress(m_hLibConv, _T("Nom_Param_Proj")); 
			m_lpfnDllNbParamMult   = (LPFNDLLNBPARAMMULT)GetProcAddress(m_hLibConv, _T("Nb_Param_Multi"));
			m_lpfnDllNomParamMult  = (LPFNDLLNOMPARAMMULT)GetProcAddress(m_hLibConv, _T("Nom_Param_Multi")); 
			m_lpfnDllLitFichier    = (LPFNDLLLITFICHIER)GetProcAddress(m_hLibConv, _T("LitFichier"));
			m_lpfnDllEcritFichier  = (LPFNDLLECRITFICHIER)GetProcAddress(m_hLibConv, _T("EcritFichier"));
			m_lpfnDllAjouterCas    = (LPFNDLLAJOUTERCAS)GetProcAddress(m_hLibConv, _T("AjouterUnCas"));
			m_lpfnDllNbEllipsoide  = (LPFNDLLNBELLIPSOIDE)GetProcAddress(m_hLibConv, _T("Nb_Ellipsoide"));
			m_lpfnDllNomEllipsoide = (LPFNDLLNOMELLIPSOIDE)GetProcAddress(m_hLibConv, _T("Nom_Ellipsoide")); 
			m_lpfnDllNbDatum       = (LPFNDLLNBDATUM)GetProcAddress(m_hLibConv, _T("Nb_Datum"));
			m_lpfnDllNomDatum      = (LPFNDLLNOMDATUM)GetProcAddress(m_hLibConv, _T("Nom_Datum")); 
			m_lpfnDllNbProjActive  = (LPFNDLLNBPROJACTIVE)GetProcAddress(m_hLibConv, _T("Nb_Projection_Active"));
			m_lpfnDllNomProjActive = (LPFNDLLNOMPROJACTIVE)GetProcAddress(m_hLibConv, _T("Nom_Projection_Active")); 
			m_lpfnDllNbMultActive  = (LPFNDLLNBMULTACTIVE)GetProcAddress(m_hLibConv, _T("Nb_Multiple_Active"));
			m_lpfnDllNomMultActive = (LPFNDLLNOMMULTACTIVE)GetProcAddress(m_hLibConv, _T("Nom_Multiple_Active")); 
			m_lpfnDllDatumToProj   = (LPFNDLLDATUMTOPROJ)GetProcAddress(m_hLibConv, _T("datum2proj")); 
			m_lpfnDllDatumToDatum  = (LPFNDLLDATUMTODATUM)GetProcAddress(m_hLibConv, _T("datum2datum")); 
			m_lpfnDllProjToDatum   = (LPFNDLLPROJTODATUM)GetProcAddress(m_hLibConv, _T("proj2datum")); 
			m_lpfnDllProjToProj    = (LPFNDLLPROJTOPROJ)GetProcAddress(m_hLibConv, _T("proj2proj")); 
			m_lpfnDllConversion    = (LPFNDLLCONVERSION)GetProcAddress(m_hLibConv, _T("Conversion")); 
			m_lpfnDllConvergence   = (LPFNDLLCONVERGENCE)GetProcAddress(m_hLibConv, _T("Convergence")); 
			m_lpfnDllAlteration    = (LPFNDLLALTERATION)GetProcAddress(m_hLibConv, _T("Alteration"));
			m_lpfnDllProjGetA      = (LPFNDLLPROJGETA)GetProcAddress(m_hLibConv, _T("Projection_GetA"));
			m_lpfnDllProjGetInvF   = (LPFNDLLPROJGETINVF)GetProcAddress(m_hLibConv, _T("Projection_GetInvF"));
			m_lpfnDllProjGetDatum  = (LPFNDLLPROJGETDATUM)GetProcAddress(m_hLibConv, _T("Projection_GetDatum"));
		}
	}

	~CConversion()
	{
		if (m_hLibConv != NULL) FreeLibrary(m_hLibConv);
	}

	int NbProjection()
	{
		if (m_lpfnDllNbProj != NULL)
			return m_lpfnDllNbProj();
		return 0;
	}

	int NomProjection(int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomProj)
			return m_lpfnDllNomProj(nIndex, lpNom, nSize);
		return 2;
	}

	int NbMultiple()
	{
		if (m_lpfnDllNbMult) 
			return m_lpfnDllNbMult();
		return 0;
	}

	int NomMultiple(int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomMult)
			return m_lpfnDllNomMult(nIndex, lpNom, nSize);
		return 2;
	}

	int NbParamProjection(int nProj)
	{
		if (m_lpfnDllNbParamProj != NULL)
			return m_lpfnDllNbParamProj(nProj);
		return 0;
	}

	int NomParamProjection(int nProj, int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomParamProj)
			return m_lpfnDllNomParamProj(nProj, nIndex, lpNom, nSize);
		return 2;
	}

	int NbParamMultiple(int nProj)
	{
		if (m_lpfnDllNbParamMult) 
			return m_lpfnDllNbParamMult(nProj);
		return 0;
	}

	int NomParamMultiple(int nProj, int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomParamMult)
			return m_lpfnDllNomParamMult(nProj, nIndex, lpNom, nSize);
		return 2;
	}

	int LitFichier(LPCTSTR lpNom, int Size)
	{
		if (m_lpfnDllLitFichier)
			return m_lpfnDllLitFichier(lpNom, Size);
		return 2;
	}

	int EcritFichier(LPCTSTR lpNom, int Size)
	{
		if (m_lpfnDllEcritFichier)
			return m_lpfnDllEcritFichier(lpNom, Size);
		return 1;
	}

	int AjouterCas(LPCTSTR lpNom, int Size)
	{
		if (m_lpfnDllAjouterCas)
			return m_lpfnDllAjouterCas(lpNom, Size);
		return 2;
	}

	int NbEllipsoide()
	{
		if (m_lpfnDllNbEllipsoide != NULL)
			return m_lpfnDllNbEllipsoide();
		return 0;
	}

	int NomEllipsoide(int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomEllipsoide)
			return m_lpfnDllNomEllipsoide(nIndex, lpNom, nSize);
		return 2;
	}

	int NbDatum()
	{
		if (m_lpfnDllNbDatum != NULL)
			return m_lpfnDllNbDatum();
		return 0;
	}

	int NomDatum(int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomDatum)
			return m_lpfnDllNomDatum(nIndex, lpNom, nSize);
		return 2;
	}

	int NbProjActive()
	{
		if (m_lpfnDllNbProjActive != NULL)
			return m_lpfnDllNbProjActive();
		return 0;
	}

	int NomProjActive(int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomProjActive)
			return m_lpfnDllNomProjActive(nIndex, lpNom, nSize);
		return 2;
	}

	int NbMultActive()
	{
		if (m_lpfnDllNbMultActive != NULL)
			return m_lpfnDllNbMultActive();
		return 0;
	}

	int NomMultActive(int nIndex, LPTSTR lpNom, int nSize)
	{
		if (m_lpfnDllNomMultActive)
			return m_lpfnDllNomMultActive(nIndex, lpNom, nSize);
		return 2;
	}

	int DatumToProj(LPCTSTR PDepart, int SizeDepart, double XD, double YD, int fuseauD, LPTSTR PArrivee, int SizeArrivee, double *XA, double *YA, int *fuseauA)
	{
		if (m_lpfnDllDatumToProj)
			return m_lpfnDllDatumToProj(PDepart, SizeDepart, XD, YD, fuseauD, PArrivee, SizeArrivee, XA, YA, fuseauA);
		return 3;
	}

	int DatumToDatum(LPCTSTR PDepart, int SizeDepart, double XD, double YD, int fuseauD, LPTSTR PArrivee, int SizeArrivee, double *LatA, double *LongA)
	{
		if (m_lpfnDllDatumToDatum)
			return m_lpfnDllDatumToDatum(PDepart, SizeDepart, XD, YD, fuseauD, PArrivee, SizeArrivee, LatA, LongA);
		return 3;
	}

	int ProjToDatum(LPCTSTR PDepart, int SizeDepart, double XD, double YD, int fuseauD, LPTSTR PArrivee, int SizeArrivee, double *LatA, double *LongA)
	{
		if (m_lpfnDllProjToDatum)
			return m_lpfnDllProjToDatum(PDepart, SizeDepart, XD, YD, fuseauD, PArrivee, SizeArrivee, LatA, LongA);
		return 3;
	}

	int ProjToProj(LPCTSTR PDepart, int SizeDepart, double XD, double YD, int fuseauD, LPTSTR PArrivee, int SizeArrivee, double *XA, double *YA, int *fuseauA)
	{
		if (m_lpfnDllProjToProj)
			return m_lpfnDllProjToProj(PDepart, SizeDepart, XD, YD, fuseauD, PArrivee, SizeArrivee, XA, YA, fuseauA);
		return 3;
	}

	BOOL Conversion(LPTSTR NomSource, int SizeNomSource, LPTSTR NomDest, int SizeNomDest,
					LPTSTR XSource, int SizeXSource, LPTSTR YSource, int SizeYSource, int IndiceSource, int *IndiceDest,
					LPTSTR XDest, int SizeXDest, LPTSTR YDest, int SizeYDest, int UniteAngle)
	{
		if (m_lpfnDllConversion)
			return m_lpfnDllConversion(NomSource, SizeNomSource, NomDest, SizeNomDest,
					XSource, SizeXSource, YSource, SizeYSource, IndiceSource, IndiceDest,
					XDest, SizeXDest, YDest, SizeYDest, UniteAngle);
		return FALSE;
	}

	BOOL Convergence(LPTSTR NomSource, int SizeNomSource, LPTSTR NomDest, int SizeNomDest,
				LPTSTR XSource, int SizeXSource, LPTSTR YSource, int SizeYSource,
				int IndiceSource, LPTSTR ResultatConv, int SizeResultatConv, int UniteAngle)
	{
		if (m_lpfnDllConvergence)
			return m_lpfnDllConvergence(NomSource, SizeNomSource, NomDest, SizeNomDest,
				XSource, SizeXSource, YSource, SizeYSource, IndiceSource, ResultatConv, SizeResultatConv, UniteAngle);
		return FALSE;
	}

	BOOL Alteration(LPTSTR NomSource, int SizeNomSource, LPTSTR NomDest, int SizeNomDest,
				LPTSTR XSource, int SizeXSource, LPTSTR YSource, int SizeYSource,
				int IndiceSource, double *ResultatAlteration)
	{
		if (m_lpfnDllAlteration)
			return m_lpfnDllAlteration(NomSource, SizeNomSource, NomDest, SizeNomDest,
				XSource, SizeXSource, YSource, SizeYSource, IndiceSource, ResultatAlteration);
		return FALSE;
	}

	BOOL ProjectionGetA(LPTSTR NomSource, int SizeNomSource, double *A)
	{
		if (m_lpfnDllProjGetA)
			return m_lpfnDllProjGetA(NomSource, SizeNomSource, A);
		return FALSE;
	}

	BOOL ProjectionGetInvF(LPTSTR NomSource, int SizeNomSource, double *A)
	{
		if (m_lpfnDllProjGetInvF)
			return m_lpfnDllProjGetInvF(NomSource, SizeNomSource, A);
		return FALSE;
	}

	BOOL ProjectionGetDatum(LPTSTR NomSource, int SizeNomSource, LPTSTR Resultat, int SizeResultat)
	{
		if (m_lpfnDllProjGetDatum)
			return m_lpfnDllProjGetDatum(NomSource, SizeNomSource, Resultat, SizeResultat);
		return FALSE;
	}

protected:
	HMODULE              m_hLibConv;

	LPFNDLLNBPROJ        m_lpfnDllNbProj;
	LPFNDLLNOMPROJ       m_lpfnDllNomProj; 
	LPFNDLLNBMULT        m_lpfnDllNbMult;
	LPFNDLLNOMMULT       m_lpfnDllNomMult; 
	LPFNDLLNBPARAMPROJ   m_lpfnDllNbParamProj;
	LPFNDLLNOMPARAMPROJ  m_lpfnDllNomParamProj; 
	LPFNDLLNBPARAMMULT   m_lpfnDllNbParamMult;
	LPFNDLLNOMPARAMMULT  m_lpfnDllNomParamMult;
	LPFNDLLLITFICHIER    m_lpfnDllLitFichier;
	LPFNDLLECRITFICHIER  m_lpfnDllEcritFichier;
	LPFNDLLAJOUTERCAS    m_lpfnDllAjouterCas;
	LPFNDLLNBELLIPSOIDE  m_lpfnDllNbEllipsoide;
	LPFNDLLNOMELLIPSOIDE m_lpfnDllNomEllipsoide; 
	LPFNDLLNBDATUM       m_lpfnDllNbDatum;
	LPFNDLLNOMDATUM      m_lpfnDllNomDatum; 
	LPFNDLLNBPROJACTIVE  m_lpfnDllNbProjActive;
	LPFNDLLNOMPROJACTIVE m_lpfnDllNomProjActive; 
	LPFNDLLNBMULTACTIVE  m_lpfnDllNbMultActive;
	LPFNDLLNOMMULTACTIVE m_lpfnDllNomMultActive; 
	LPFNDLLDATUMTOPROJ   m_lpfnDllDatumToProj; 
	LPFNDLLDATUMTODATUM  m_lpfnDllDatumToDatum; 
	LPFNDLLPROJTOPROJ    m_lpfnDllProjToProj;
	LPFNDLLPROJTODATUM   m_lpfnDllProjToDatum; 
	LPFNDLLCONVERSION    m_lpfnDllConversion;
	LPFNDLLCONVERGENCE   m_lpfnDllConvergence; 
	LPFNDLLALTERATION    m_lpfnDllAlteration; 
	LPFNDLLPROJGETA      m_lpfnDllProjGetA;
	LPFNDLLPROJGETINVF   m_lpfnDllProjGetInvF;
	LPFNDLLPROJGETDATUM  m_lpfnDllProjGetDatum;

}; // class CConversion

} // namespace GeoConversion
