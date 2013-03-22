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
@Constraint(validatedBy = Hostmask.NetmaskValidator.class)
@Documented
public @interface Hostmask {

    String message();

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class NetmaskValidator implements ConstraintValidator<Hostmask, String> {

        @Override
        public void initialize(Hostmask constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                return Mask.parse(value).isHostmask();
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
