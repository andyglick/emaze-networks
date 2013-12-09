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
import net.emaze.networks.Ipv4Network;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = Ipv4Cidr.Validator.class)
@Documented
public @interface Ipv4Cidr {

    String message() default "Non è un CIDR valido";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class Validator implements ConstraintValidator<Ipv4Cidr, String> {

        @Override
        public void initialize(Ipv4Cidr constraintAnnotation) {}

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                Ipv4Network.fromCidrNotation(value);
                return true;
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
