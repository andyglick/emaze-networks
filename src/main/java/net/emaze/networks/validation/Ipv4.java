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

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = Ipv4.Validator.class)
@Documented
public @interface Ipv4 {

    String message() default "Non è un indirizzo IP valido";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class Validator implements ConstraintValidator<Ipv4, String> {

        @Override
        public void initialize(Ipv4 constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                net.emaze.networks.ipv4.Ipv4.parse(value);
                return true;
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
