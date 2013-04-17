package net.emaze.networks.dozer;

import org.dozer.BeanFactory;

public class ImmutableBeanFactory implements BeanFactory {

    @Override
    public Object createBean(Object source, Class<?> sourceClass, String targetBeanId) {
        return source;
    }
}
