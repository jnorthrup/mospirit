.PHONY: clean All

All:
	@echo "----------Building project:[ mlite - Debug ]----------"
	@$(MAKE) -f  "mlite.mk"
clean:
	@echo "----------Cleaning project:[ mlite - Debug ]----------"
	@$(MAKE) -f  "mlite.mk" clean
