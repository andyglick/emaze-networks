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
import net.emaze.networks.Mask;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = Ipv4Mask.Validator.class)
@Documented
public @interface Ipv4Mask {

    String message() default "Non è una netmask valida";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class Validator implements ConstraintValidator<Ipv4Mask, String> {

        @Override
        public void initialize(Ipv4Mask constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                Mask.netV4(value);
                return true;
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
