$(part)_ML_FILES  := $(call get-ml-files, $($(part)_SRC_FILES))
$(part)_MLI_FILES := $(call get-mli-files,$($(part)_SRC_FILES))
$(part)_CMX_FILES := $($(part)_ML_FILES:%.ml=%.cmx) 
$(part)_CMO_FILES := $($(part)_ML_FILES:%.ml=%.cmo)
$(part)_CMI_FILES := $($(part)_MLI_FILES:%.mli=%.cmi) $($(part)_ML_FILES:%.ml=%.cmi)
SRC_FILES      	  := $(SRC_FILES) $($(part)_SRC_FILES)
GARBAGE        	  := $(GARBAGE)					\
		     $($(part)_ML_FILES)			\
		     $($(part)_MLI_FILES)			\
		     $($(part)_CMX_FILES)			\
		     $($(part)_CMO_FILES)			\
		     $($(part)_CMI_FILES)			\
		     $($(part)_CMX_FILES:%.cmx=%.o)	
ML_FILES  := $(ML_FILES)  $($(part)_ML_FILES)
MLI_FILES := $(MLI_FILES) $($(part)_MLI_FILES)

bin/$(part): $($(part)_CMX_FILES) | bin/
