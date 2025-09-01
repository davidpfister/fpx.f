#define KWM a
#define FLM(x) b FLM2(x) KWM c
#define FLM2(x) d FLM(x) e
KWM
FLM(y)
FLM(KWM)
FLM(FLM(y))
FLM(FLM(KWM))