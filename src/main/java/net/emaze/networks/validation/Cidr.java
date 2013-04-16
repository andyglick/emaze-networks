package net.emaze.networks.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import javax.validation.Payload;
import net.emaze.networks.Network;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = Cidr.CidrValidator.class)
@Documented
public @interface Cidr {

    String message() default "Non è un CIDR valido";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class CidrValidator implements ConstraintValidator<Cidr, String> {

        @Override
        public void initialize(Cidr constraintAnnotation) {}

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                Network.fromCidrNotation(value);
                return true;
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
