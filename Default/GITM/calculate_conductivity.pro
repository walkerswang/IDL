function calculate_conductivity, den_e,den_N2,den_O2,den_O,Ti,Te,Tn,Rion,B

  ; this program is used to calculate the ionospheric conductivity
  ; using measurements from incoherent scatter radars
  ; mu is ion mass/proton mass; Z is the ion charge
  ;Rion is ratio of each ion species
  ;constant
  q=1.602e-19; %unit coulombs

  ;ion-neutral collison frequency (s-1)
  Ti_mean=Ti;
  ;ion temperature should be the same for all ion species
  Tr=(Ti_mean+Tn)/2;

  ;collision frequency for O+ and neutrals
  ;1.2 times the O+-O collision frequency according to Davis and Lester [1999], AG
  Mu_O=6.82e-16*den_N2+6.64e-16*den_O2+1.2*3.67e-17*den_O*sqrt(Tr)*(1-0.064*alog10(Tr))^2;

  ;collision frequency for O2+ and neutrals
  Mu_O2=4.13e-16*den_N2+2.31e-16*den_O+2.59e-17*den_O2*sqrt(Tr)*(1-0.073*alog10(Tr))^2;

  ;collision frequency for NO+ and neutrals
  Mu_NO=4.34e-16*den_N2+4.27e-16*den_O2+2.44e-16*den_O;

  ;Mu_in based on Brekke and Moen, 1993
  ;Mu_in1=4.23e-16*(den_N2+(4.18/4.23)*den_O2+(2.38/4.23)*den_O) ;number density in m-3
  ;Mu_in2=6.82e-16*(den_N2+(6.66/6.82)*den_O2+(3.42/6.82)*den_O*sqrt(Tr)*(1.08-0.139)*alog10(Tr)$
  ;       +(4.51/6.82)*1e-3*(alog10(Tr))^2)

  ;electron-neutral collison frequency (s-1)
  mm1=2.33e-17*den_N2*(1-1.21e-4*Te)*Te;
  mm2=1.82e-16*den_O2*(1+3.6e-2*sqrt(Te))*sqrt(Te);
  mm3=8.9e-17*den_O*(1+5.7e-4*Te)*sqrt(Te);
  Mu_en=mm1+mm2+mm3;

  ;ion gyro-frequency (rad/s)
  Omega_O=9.5819e7*B/16.0;
  ;O+, O2+, NO+, N2+, N+
  Omega_O2=9.5819e7*B/32.0;
  Omega_NO=9.5819e7*B/30.0;

  ;electron gyro-frequency (rad/s)
  Omega_e=double(1.7584e11*B);

  tm0=den_e*q/B;
  tm1=Omega_e*Mu_en/(Omega_e^2+Mu_en^2);
  tm2_O=Omega_O*Mu_O/(Omega_O^2+Mu_O^2);
  tm2_O2=Omega_O2*Mu_O2/(Omega_O2^2+Mu_O2^2);
  tm2_NO=Omega_NO*Mu_NO/(Omega_NO^2+Mu_NO^2);
  Sigma_P=tm0*(tm1+Rion(0)*tm2_O+Rion(1)*tm2_O2+Rion(2)*tm2_NO);

  tm3=Omega_e^2/(Omega_e^2+Mu_en^2);
  tm4_O=Omega_O^2/(Omega_O^2+Mu_O^2);
  tm4_O2=Omega_O2^2/(Omega_O2^2+Mu_O2^2);
  tm4_NO=Omega_NO^2/(Omega_NO^2+Mu_NO^2);
  Sigma_H=tm0*(tm3-Rion(0)*tm4_O-Rion(1)*tm4_O2-Rion(2)*tm4_NO);

  sigma=fltarr(2,1)
  sigma[0]=Sigma_P
  sigma[1]=Sigma_H
  return,sigma
  
 end