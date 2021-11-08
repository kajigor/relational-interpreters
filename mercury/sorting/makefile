MMC = mmc
FLAG = make
EFLAG = E
INPUT = sorting
BUILD_DIR = Mercury

all: clean build

err: clean builderr

builderr:
	$(MMC) --$(FLAG) -$(EFLAG) $(INPUT)

build:
	$(MMC) --$(FLAG) $(INPUT)

clean:
	rm -f $(INPUT).err $(INPUT).mh $(INPUT)
	rm -rf $(BUILD_DIR)

