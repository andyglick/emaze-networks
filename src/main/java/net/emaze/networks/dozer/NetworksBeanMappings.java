
package net.emaze.networks.dozer;

import net.emaze.networks.Ipv4;
import net.emaze.networks.Ipv4Mask;
import net.emaze.networks.Ipv4Network;
import net.emaze.networks.Ipv6;
import net.emaze.networks.Ipv6Mask;
import net.emaze.networks.Ipv6Network;
import org.dozer.loader.api.BeanMappingBuilder;
import org.dozer.loader.api.TypeMappingOptions;


public class NetworksBeanMappings extends BeanMappingBuilder {

    @Override
    protected void configure() {
        mapping(Ipv4.class, Ipv4.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv4Network.class, Ipv4Network.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv4Mask.class, Ipv4Mask.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv6.class, Ipv6.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv6Network.class, Ipv6Network.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
        mapping(Ipv6Mask.class, Ipv6Mask.class, TypeMappingOptions.beanFactory(ImmutableBeanFactory.class.getCanonicalName()));
    }

}
