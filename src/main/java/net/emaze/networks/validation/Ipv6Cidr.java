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
import net.emaze.networks.ipv6.Ipv6Network;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = Ipv6Cidr.Validator.class)
@Documented
public @interface Ipv6Cidr {

    String message() default "Non è un CIDR valido";

    int maxPopulation() default 128;

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    public static class Validator implements ConstraintValidator<Ipv6Cidr, String> {

        private Ipv6Cidr instance;

        @Override
        public void initialize(Ipv6Cidr constraintAnnotation) {
            this.instance = constraintAnnotation;
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            try {
                final Ipv6Network network = Ipv6Network.fromCidrNotation(value);
                return network.netmask().population() <= instance.maxPopulation();
            } catch (Exception ex) {
                return false;
            }
        }
    }
}
