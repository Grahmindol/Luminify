OCAMLOPT = ocamlopt
TARGET = bin/main
OBJDIR = bin/obj

SRCS = main.ml
OBJS = $(OBJDIR)/main.cmx

all: $(TARGET)

# Créer les dossiers si besoin
$(OBJDIR):
	mkdir -p $(OBJDIR)

bin:
	mkdir -p bin

# Compile le .ml en .cmx dans OBJDIR
$(OBJDIR)/main.cmx: main.ml | $(OBJDIR)
	$(OCAMLOPT) -c -o $(OBJDIR)/main.cmx main.ml

# Link les objets pour créer l'exécutable
$(TARGET): bin $(OBJS)
	$(OCAMLOPT) -o $(TARGET) $(OBJS)

clean:
	rm -rf bin

