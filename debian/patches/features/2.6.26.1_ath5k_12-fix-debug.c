diff --git a/drivers/net/wireless/ath5k/debug.c b/drivers/net/wireless/ath5k/debug.c
index 41d5fa3..6fa6c8e 100644
--- a/drivers/net/wireless/ath5k/debug.c
+++ b/drivers/net/wireless/ath5k/debug.c
@@ -129,7 +129,7 @@ static struct reg regs[] = {
 	REG_STRUCT_INIT(AR5K_CPC1),
 	REG_STRUCT_INIT(AR5K_CPC2),
 	REG_STRUCT_INIT(AR5K_CPC3),
-	REG_STRUCT_INIT(AR5K_CPCORN),
+	REG_STRUCT_INIT(AR5K_CPCOVF),
 	REG_STRUCT_INIT(AR5K_RESET_CTL),
 	REG_STRUCT_INIT(AR5K_SLEEP_CTL),
 	REG_STRUCT_INIT(AR5K_INTPEND),
