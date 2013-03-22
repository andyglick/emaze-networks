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
import net.emaze.networks.Ip;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = Ipv4.Ipv4Validator.class)
@Documented
public @interface Ipv4 {

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class Ipv4Validator implements ConstraintValidator<Ipv4, String> {

        @Override
        public void initialize(Ipv4 constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                Ip.parse(value);
                return true;
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
