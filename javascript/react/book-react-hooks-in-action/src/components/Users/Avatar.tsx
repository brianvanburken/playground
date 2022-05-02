import { Suspense } from "react";
import { useQuery } from "react-query";

interface AvatarProps {
  src: string;
  alt: string;
  fallbackSrc: string;
  [key: string]: unknown;
}

export default function Avatar({
  src,
  alt,
  fallbackSrc,
  ...props
}: AvatarProps) {
  return (
    <div className="user-avatar">
      <Suspense fallback={<img src={fallbackSrc} alt="Fallback Avatar" />}>
        <Img src={src} alt={alt} {...props} />
      </Suspense>
    </div>
  );
}

interface ImgProps {
  src: string;
  alt: string;
  [key: string]: string;
}

function Img({ src, alt, ...props }: ImgProps) {
  const { data: imgObject } = useQuery<HTMLImageElement>(
    src,
    () =>
      new Promise((resolve) => {
        const img = new Image();
        img.onload = () => resolve(img);
        img.src = src;
      }),
    { suspense: true }
  );
  return <img src={imgObject?.src} alt={alt} {...props} />;
}
